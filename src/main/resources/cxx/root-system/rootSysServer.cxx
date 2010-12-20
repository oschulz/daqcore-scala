// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#include <iostream>
#include <iomanip>
#include <cstring>
#include <vector>
#include <cassert>
#include <map>
#include <list>

#include <TSystem.h>
#include <TFile.h>
#include <TTree.h>
#include <TMessage.h>


using namespace std;


void log_trace(const char *msg) { cerr << "TRACE: " << msg << endl << flush; }
void log_debug(const char *msg) { cerr << "DEBUG: " << msg << endl << flush; }
void log_info(const char *msg) { cerr << "INFO: " << msg << endl << flush; }
void log_warn(const char *msg) { cerr << "WARN: " << msg << endl << flush; }
void log_error(const char *msg) { cerr << "ERROR: " << msg << endl << flush; }

#ifdef ERROR
#define error(arg) log_error(arg)
#define WARN
#else
#define error(arg)
#endif

#ifdef WARN
#define warn(arg) log_warn(arg)
#define INFO
#else
#define warn(arg)
#endif

#ifdef INFO
#define info(arg) log_info(arg)
#define DEBUG
#else
#define info(arg)
#endif

#ifdef DEBUG
#define debug(arg) log_debug(arg)
#define TRACE
#else
#define debug(arg)
#endif

#ifdef TRACE
#define trace(arg) log_trace(arg)
#else
#define trace(arg)
#endif



const TString version("0.1.0");

const UChar_t msgHeader[2] = {UChar_t(0x10), UChar_t(0x9B)}; // DLE CSI
const UInt_t MSG_REQUEST = 0x52455155;
const UInt_t MSG_RESPONSE = 0x52455350;

void shutdown();


class Messaging {
public:
	static UInt_t netToHost(UInt_t i) {
		UChar_t *b = (UChar_t*) &i;
		return (UInt_t(b[0]) << 24) | (UInt_t(b[1]) << 16) | (UInt_t(b[2]) << 8) | (UInt_t(b[3]) << 0);
	}

	static UInt_t hostToNet(UInt_t i) { return netToHost(i); }
};



class Input : public Messaging {
protected:
	std::istream *m_in;
	
	void read(void *buffer, size_t count, bool closeOnFail = false) {
		trace(Form("Reading %i bytes from input", count));
		m_in->read((char*)buffer, count);
		if (!m_in->good() && closeOnFail) {
			debug("End of input, exiting");
			shutdown();
		}
		else assert( m_in->good() );
	}

public:
	void recv(TMessage &msg) {
		trace("Waiting for message header");
		UChar_t header[2];
		read(header, sizeof(header), true);
		assert ((header[0] == msgHeader[0]) && (header[0] == msgHeader[0]));
	
		// Have to do it this way because constructor
		// TMessage(void* buf, Int_t bufsize) is protected.
		msg.Reset();
		msg.ResetMap();
		msg.SetWhat(0);
		UInt_t len = 0;
		Int_t msgType = 0;
		read(&len, sizeof(len));
		len = netToHost(len);
		if (len < sizeof(msgType)) return;
		
		UChar_t *buffer = new UChar_t[len];
		read(buffer, len);
		// cerr << "TRACE: In: "; for (UInt_t i=0; i < len; ++i) cerr << " " << hex << setw(2) << setfill('0') << UInt_t(UChar_t(buffer[i])); cerr << endl;
		msgType = hostToNet(*((Int_t*)(buffer)));
		msg.SetWhat(msgType);
		if (len > sizeof(msgType)) msg.WriteFastArray(buffer+sizeof(msgType), len-sizeof(msgType));
		delete [] buffer;
		msg.SetReadMode();
		msg.SetBufferOffset(8);

		trace(Form("Received message of length %i, what = %i", msg.Length(), msg.What()));
		buffer = (UChar_t *) msg.Buffer();
	}

	Input(std::istream *in) : m_in(in) { }
};


class Output : public Messaging {
protected:
	std::ostream *m_out;
	
	void write(const void *buffer, size_t count) {
		m_out->write((char*)buffer, count);
		assert( m_out->good() );
	}
	void flush() { m_out->flush(); }

public:
	void send(const TMessage &msg) {
		char *buffer = msg.Buffer();
		Int_t length = msg.Length();
		if (msg.CompBuffer()) {
		  buffer = msg.CompBuffer();
		  length = msg.CompLength();
		}

		// Can't use msg.SetLength() - not supported by CINT for some reason
		*(Int_t*)(buffer) = hostToNet(length - sizeof(Int_t));

		write(msgHeader, sizeof(msgHeader));
		write(buffer, length);
		flush();

		// cerr << "TRACE: Out: "; for (Int_t i=0; i < length; ++i) cerr << " " << hex << setw(2) << setfill('0') << UInt_t(UChar_t(buffer[i])); cerr << endl;
		trace(Form("Sent message of length %i, what = %i", msg.Length(), msg.What()));
	}

	Output(std::ostream *out) : m_out(out) { }
};



class Value {
public:
	virtual TString type() const = 0;
	virtual void createBranch(TTree &tree, const TString &name) = 0;
	virtual void openBranch(TTree &tree, const TString &name) = 0;
	virtual void writeTo(TMessage &msg) const = 0;
	virtual void readFrom(TMessage &msg) = 0;
	virtual TString toString() const = 0;
	virtual ~Value() {}
};


class AtomicValue : public Value {
protected:
	TString m_type;

public:
	TString type() const { return m_type; }
	
	AtomicValue(const TString &type): m_type(type) {  }
};


class Field {
protected:
	TString m_name;
	Value* m_value;

public:
	const TString& name() const { return m_name; }

	const Value& value() const { return *m_value; }
	Value& value() { return *m_value; }

	virtual void createBranch(TTree &tree) { value().createBranch(tree, name()); }
	virtual void openBranch(TTree &tree) { value().openBranch(tree, name()); }

	virtual TString toString() const { return name() + ": " + value().toString(); }
	
	Field(const TString &name, Value* value): m_name(name), m_value(value) {  }
	virtual ~Field() { delete m_value; }
};

std::ostream& operator<<(std::ostream &out, const Field &field) {
	out << field.name() << " " << field.value().type();
	return out;
}


class BoolValue: public AtomicValue {
public:
	Bool_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteBool(value); }
	void readFrom(TMessage &msg) { msg.ReadBool(value); }
	TString toString() const { return TString(value ? "true" : "false"); }
	BoolValue() : AtomicValue("bool"), value(false) {}
};


class Int8Value: public AtomicValue {
public:
	Char_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteChar(value); }
	void readFrom(TMessage &msg) { msg.ReadChar(value); }
	TString toString() const { return TString(Form("%i", (int)(value))); }
	Int8Value() : AtomicValue("int16"), value(0) {}
};


class Int16Value: public AtomicValue {
public:
	Short_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteShort(value); }
	void readFrom(TMessage &msg) { msg.ReadShort(value); }
	TString toString() const { return TString(Form("%i", (int)(value))); }
	Int16Value() : AtomicValue("int16"), value(0) {}
};


class Int32Value: public AtomicValue {
public:
	Int_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value); }
	void readFrom(TMessage &msg) { msg.ReadInt(value); }
	TString toString() const { return TString(Form("%li", (long)(value))); }
	Int32Value() : AtomicValue("int32"), value(0) {}
};


class Int64Value: public AtomicValue {
public:
	Long_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteLong(value); }
	void readFrom(TMessage &msg) { msg.ReadLong(value); }
	TString toString() const { return TString(Form("%lli", (long long)(value))); }
	Int64Value() : AtomicValue("int64"), value(0) {}
};


class FloatValue: public AtomicValue {
public:
	Float_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteFloat(value); }
	void readFrom(TMessage &msg) { msg.ReadFloat(value); }
	TString toString() const { return TString(Form("%f", float(value))); }
	FloatValue() : AtomicValue("float"), value(0) {}
};


class DoubleValue: public AtomicValue {
public:
	Double_t value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteDouble(value); }
	void readFrom(TMessage &msg) { msg.ReadDouble(value); }
	TString toString() const { return TString(Form("%lf", double(value))); }
	DoubleValue() : AtomicValue("double"), value(0) {}
};


class StringValue: public AtomicValue {
public:
	TString *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value, 32000, 0); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteTString(*value); }
	void readFrom(TMessage &msg) { msg.ReadTString(*value); }
	TString toString() const { return TString(*value); }
	StringValue() : AtomicValue("string"), value(new TString) {}
	~StringValue() { if (value != 0) delete value; }
};


class UUIDValue: public AtomicValue {
public:
	TUUID *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value, 32000, 0); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const {
		unsigned long f1; unsigned int f2, f3, f4; unsigned long f5a; unsigned int f5b;
		sscanf(value->AsString(), "%08lx-%04x-%04x-%04x-%08lx%04x", &f1, &f2, &f3, &f4, &f5a, &f5b);
		UInt_t i1=f1; UShort_t i2=f2, i3=f3, i4=f4; UInt_t i5a=f5a; UShort_t i5b=f5b;
		msg.WriteUInt(i1); msg.WriteUShort(i2); msg.WriteUShort(i3); msg.WriteUShort(i4); msg.WriteUInt(i5a); msg.WriteUShort(i5b);
	}
	void readFrom(TMessage &msg) {
		char str[40];
		UInt_t i1; UShort_t i2, i3, i4; UInt_t i5a; UShort_t i5b;
		msg.ReadUInt(i1); msg.ReadUShort(i2); msg.ReadUShort(i3); msg.ReadUShort(i4); msg.ReadUInt(i5a); msg.ReadUShort(i5b);
		unsigned long f1=i1; unsigned int f2=i2, f3=i3, f4=i4; unsigned long f5a=i5a; unsigned int f5b=i5b;
		sprintf(str, "%08lx-%04x-%04x-%04x-%08lx%04x", f1, f2, f3, f4, f5a, f5b);
		*value = TUUID(str);
	}
	TString toString() const { return TString(value->AsString()); }
	UUIDValue() : AtomicValue("uuid"), value(new TUUID) {}
	~UUIDValue() { if (value != 0) delete value; }
};


/*class BoolVectorValue: public AtomicValue {
public:
	vector<Bool_t> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<bool>(size = %li)", (long)(value->size()))); }
	BoolVectorValue() : AtomicValue("vector<int8>"), value(new vector<Bool_t>) {}
	~BoolVectorValue() { if (value != 0) delete value; }
};*/


class CharVectorValue: public AtomicValue {
public:
	vector<Char_t> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<int8>(size = %li)", (long)(value->size()))); }
	CharVectorValue() : AtomicValue("vector<int8>"), value(new vector<Char_t>) {}
	~CharVectorValue() { if (value != 0) delete value; }
};


class ShortVectorValue: public AtomicValue {
public:
	vector<Short_t> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<int16>(size = %li)", (long)(value->size()))); }
	ShortVectorValue() : AtomicValue("vector<int16>"), value(new vector<Short_t>) {}
	~ShortVectorValue() { if (value != 0) delete value; }
};


class IntVectorValue: public AtomicValue {
public:
	vector<Int_t> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<int32>(size = %li)", (long)(value->size()))); }
	IntVectorValue() : AtomicValue("vector<int32>"), value(new vector<Int_t>) {}
	~IntVectorValue() { if (value != 0) delete value; }
};


class LongVectorValue: public AtomicValue {
public:
	vector<Long_t> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<int64>(size = %li)", (long)(value->size()))); }
	LongVectorValue() : AtomicValue("vector<int64>"), value(new vector<Long_t>) {}
	~LongVectorValue() { if (value != 0) delete value; }
};


class FloatVectorValue: public AtomicValue {
public:
	vector<float> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<float>(size = %li)", (long)(value->size()))); }
	FloatVectorValue() : AtomicValue("vector<float>"), value(new vector<float>) {}
	~FloatVectorValue() { if (value != 0) delete value; }
};


class DoubleVectorValue: public AtomicValue {
public:
	vector<double> *value;
	void createBranch(TTree &tree, const TString &name) { tree.Branch(name.Data(), &value); }
	void openBranch(TTree &tree, const TString &name) { tree.SetBranchAddress(name.Data(), &value); }
	void writeTo(TMessage &msg) const { msg.WriteInt(value->size()); if (value->size() > 0) msg.WriteFastArray(&(value->at(0)), value->size()); }
	void readFrom(TMessage &msg) { Int_t size = 0; msg.ReadInt(size); if (size > Int_t(value->capacity())) value->reserve(size); value->resize(size); if (size > 0) msg.ReadFastArray(&(value->at(0)), size); }
	TString toString() const { return TString(Form("vector<double>(size = %li)", (long)(value->size()))); }
	DoubleVectorValue() : AtomicValue("vector<double>"), value(new vector<double>) {}
	~DoubleVectorValue() { if (value != 0) delete value; }
};


Value *newValue(const TString &type) {
	if (type == "bool") return new BoolValue;
	if (type == "int8") return new Int8Value;
	if (type == "int16") return new Int16Value;
	if (type == "int32") return new Int32Value;
	if (type == "int64") return new Int64Value;
	if (type == "float") return new FloatValue;
	if (type == "double") return new DoubleValue;
	if (type == "string") return new StringValue;
	if (type == "uuid") return new UUIDValue;
	// if (type == "vector<bool>") return new BoolVectorValue;
	if (type == "vector<int8>") return new CharVectorValue;
	if (type == "vector<int16>") return new ShortVectorValue;
	if (type == "vector<int32>") return new IntVectorValue;
	if (type == "vector<int64>") return new LongVectorValue;
	if (type == "vector<float>") return new FloatVectorValue;
	if (type == "vector<double>") return new DoubleVectorValue;
	else assert(false);
}


std::ostream& operator<<(std::ostream &out, const TMessage &msg) {
	char *buffer = msg.Buffer();
	Int_t length = msg.Length();
	out << "[";
	for (Int_t i=0; i < length; ++i) out << (i > 0 ? " " : "") << hex << setw(2) << setfill('0') << UInt_t(UChar_t(buffer[i]));
	out << "]";
	return out;
}



class ActiveTree {
protected:
	typedef list<void*>::iterator Iterator;
	typedef list<void*>::const_iterator ConstIterator;
	TTree *m_tree;
	list<void*> m_fields;

public:
	const TTree& tree() const { return *m_tree; }
	TTree& tree() { return *m_tree; }

	Int_t size() const { return m_tree->GetEntriesFast(); }

	void createBranch(const TString &name, Value* value) {
		trace(Form("ActiveTree::createBranch(%s, %s)", name.Data(), value->type().Data()));
		Field *field = new Field(name, value);
		field->createBranch(*m_tree);
		m_fields.push_back(field);
	}

	void openBranch(const TString &name, Value* value) {
		trace(Form("ActiveTree::openBranch(%s, %s)", name.Data(), value->type().Data()));
		Field *field = new Field(name, value);
		field->openBranch(*m_tree);
		m_fields.push_back(field);
	}
	
	void addEntry(TMessage &req) {
		trace(Form("ActiveTree::addTreeEntry(...)"));
		for (Iterator it = m_fields.begin(); it != m_fields.end(); ++it) {
			Field &field = *(Field*)(*it);
			field.value().readFrom(req);
			trace(Form("Field %s", field.toString().Data()));
		}
		m_tree->Fill();
	}

	void getEntry(Int_t index, TMessage &resp) {
		trace(Form("ActiveTree::getTreeEntry(%i)", index));
		assert(index < size());
		Int_t nBytes = m_tree->GetEntry(index);
		assert(nBytes > 0);
		trace(Form("Read %li bytes from tree entry", (long)(nBytes)));
		for (Iterator it = m_fields.begin(); it != m_fields.end(); ++it) {
			Field &field = *(Field*)(*it);
			trace(Form("Field %s", field.toString().Data()));
			field.value().writeTo(resp);
		}
	}
	
	std::ostream& print(std::ostream &out) const {
		out << "\"" << tree().GetName() << "\"" << " {" << endl;
		for (ConstIterator it = m_fields.begin(); it != m_fields.end(); ++it) {
			Field &field = *(Field*)(*it);
			out << "    " << field << ";" << endl;
		}
		out << "}" << endl;
		return out;
	}	
	
	ActiveTree(TTree *tree): m_tree(tree) {}
	
	virtual ~ActiveTree() {
		Iterator it = m_fields.begin();
		while (it != m_fields.end()) {
			delete (Field*)(*it);
			m_fields.erase(++it);
		}
	}
};

std::ostream& operator<<(std::ostream &out, const ActiveTree &tree) { return tree.print(out); }


typedef map<long, void*> PtrMap;

class TTreeMap {
protected:
	PtrMap m_map;

public:
	bool validId(Int_t id)
		{ return (m_map.find((long)id) != m_map.end()); }

	ActiveTree* operator[](Int_t id) {
		assert( validId(id) );
		return (ActiveTree*)(m_map[(long)id]);
	}
	
	void add(Int_t id, ActiveTree* value) {
		assert( !validId(id) );
		m_map[(long)id] = value;
		assert (validId(id) );
	}
	
	void del(Int_t id) {
		m_map.erase(id);
	}
	
	void delAllInFile(TFile* file) {
		PtrMap::iterator it = m_map.begin();
		while (it != m_map.end()) {
			if (((ActiveTree*)it->second)->tree().GetCurrentFile() == file)
				m_map.erase(it++);
			else ++it;
		}
	}

	virtual ~TTreeMap() {}
};

TTreeMap m_trees;


class TFileMap {
protected:
	PtrMap m_map;
	
	void closeFile(TFile* file) {
		debug(Form("Closing TFile \"%s\"", file->GetName()));
		file->Write();
		file->Close();
		delete file;
	}

public:
	bool validId(Int_t id)
		{ return (m_map.find((long)id) != m_map.end()); }

	TFile* operator[](Int_t id) {
		assert( validId(id) );
		return (TFile*)(m_map[(long)id]);
	}
	
	void add(Int_t id, TFile* value) {
		assert( !validId(id) );
		m_map[(long)id] = value;
	}
	
	void del(Int_t id) {
		TFile *file = (*this)[id];
		m_map.erase(id);
		m_trees.delAllInFile(file);
		closeFile(file);
	}
	
	void clear() {
		for (PtrMap::iterator it = m_map.begin(); it != m_map.end(); ++it)
			closeFile(this->operator[](it->first));
		m_map.clear();
	}
	
	virtual ~TFileMap() {}
};

TFileMap m_tfiles;


void srvGetIdn(TMessage &resp) {
	debug("getIdn()");

	TString manufacturer("DAQCorE");
	TString model("ROOT-System-Server");
	TString hwversion(gSystem->GetBuildArch());
	TString swversion(version);
	
	resp.WriteTString(manufacturer);
	resp.WriteTString(model);
	resp.WriteTString(hwversion);
	resp.WriteTString(swversion);
}


void srvOpenTFile(TMessage &req) {
	Int_t id; req.ReadInt(id); // ID to assign to TFile
	TString name; req.ReadTString(name);
	TString mode; req.ReadTString(mode);
	debug(Form("openTFile(%i, %s, %s)", id, name.Data(), mode.Data()));

	TDirectory *currentDir = gDirectory;
	m_tfiles.add(id, new TFile(name.Data(), mode.Data()));
	currentDir->cd();
}


void srvCloseTFile(TMessage &req) {
	Int_t id; req.ReadInt(id);
	debug(Form("closeTFile(%i)", id));

	m_tfiles.del(id);
}


void srvCreateTree(TMessage &req) {
	Int_t id; req.ReadInt(id); // ID to assign to TTree
	Int_t fileId; req.ReadInt(fileId);
	TString name; req.ReadTString(name);
	TString title; req.ReadTString(title);
	debug(Form("createTree(%i, %i, %s, %s)", id, fileId, name.Data(), title.Data()));
	
	TDirectory *currentDir = gDirectory;
	TFile *file = m_tfiles[fileId];
	file->cd();
	TTree *tree = new TTree(name.Data(), title.Data());
	tree->SetDirectory(file); // Just to make extra sure ...
	currentDir->cd();
	m_trees.add(id, new ActiveTree(tree));
}


void srvOpenTree(TMessage &req) {
	Int_t id; req.ReadInt(id); // ID to assign to TTree
	Int_t fileId; req.ReadInt(fileId);
	TString name; req.ReadTString(name);
	debug(Form("openTree(%i, %i, %s)", id, fileId, name.Data()));
	
	TFile *file = m_tfiles[fileId];
	assert(file != 0);
	TTree *tree = 0;
	file->GetObject(name.Data(), tree);
	assert(tree != 0);
	m_trees.add(id, new ActiveTree(tree));
}


void srvGetTreeSize(TMessage &req, TMessage &resp) {
	Int_t treeId; req.ReadInt(treeId);

	const Int_t logEvery = 1000; static int counter = 0;
	if (counter % logEvery == 0) { debug(Form("getTreeSize(%i) [logging every %i's call]", treeId, logEvery)); }
	++counter;
	
	ActiveTree *tree = m_trees[treeId];
	Int_t size = tree->size();

	resp.WriteInt(size);
}


void srvCreateBranch(TMessage &req) {
	Int_t treeId; req.ReadInt(treeId);
	TString name; req.ReadTString(name);
	TString type; req.ReadTString(type);
	debug(Form("createBranch(%i, %s, %s)", treeId, name.Data(), type.Data()));
	
	ActiveTree *tree = m_trees[treeId];
	
	tree->createBranch(name, newValue(type));
}


void srvOpenBranch(TMessage &req) {
	Int_t treeId; req.ReadInt(treeId);
	TString name; req.ReadTString(name);
	TString type; req.ReadTString(type);
	debug(Form("openBranch(%i, %s, %s)", treeId, name.Data(), type.Data()));
	
	ActiveTree *tree = m_trees[treeId];
	
	tree->openBranch(name, newValue(type));
}


void srvAddTreeEntry(TMessage &req) {
	Int_t treeId; req.ReadInt(treeId);

	const Int_t logEvery = 1000; static int counter = 0;
	if (counter % logEvery == 0) { debug(Form("addTreeEntry(%i, ...) [logging every %i's call]", treeId, logEvery)); }
	++counter;
	
	ActiveTree *tree = m_trees[treeId];
	tree->addEntry(req);
}


void srvGetTreeEntry(TMessage &req, TMessage &resp) {
	Int_t treeId; req.ReadInt(treeId);
	Int_t index; req.ReadInt(index);

	const Int_t logEvery = 1000; static int counter = 0;
	if (counter % logEvery == 0) { debug(Form("getTreeEntry(%i, %i) [logging every %i's call]", treeId, index, logEvery)); }
	++counter;
	
	ActiveTree *tree = m_trees[treeId];
	tree->getEntry(index, resp);
}


void processInstruction(TMessage &req, TMessage &resp) {
	assert( resp.What() == MSG_RESPONSE );
	TString header; req.ReadTString(header);

	trace(Form("Received instruction %s", header.Data()));
	
	if      (header == "GetIdn") srvGetIdn(resp);
	else if (header == "OpenTFile") srvOpenTFile(req);
	else if (header == "CloseTFile") srvCloseTFile(req);
	else if (header == "CreateTree") srvCreateTree(req);
	else if (header == "OpenTree") srvOpenTree(req);
	else if (header == "GetTreeSize") srvGetTreeSize(req, resp);
	else if (header == "CreateBranch") srvCreateBranch(req);
	else if (header == "OpenBranch") srvOpenBranch(req);
	else if (header == "AddTreeEntry") srvAddTreeEntry(req);
	else if (header == "GetTreeEntry") srvGetTreeEntry(req, resp);
	else assert(false);
}


void shutdown() {
	info("Shutting down");
	m_tfiles.clear();
	exit(0);
}


void rootSysServer() {
	info("Ready");

	Input in(&cin);
	Output out(&cout);
	
	TString name;
	TString title;

	TMessage req(0);
	TMessage resp(0);

	while (cin.good()) {
		in.recv(req);
		assert(req.What() == MSG_REQUEST);
		int reqIdx = 0;
		req.ReadInt(reqIdx);

		resp.Reset();
		resp.ResetMap();
		resp.SetWhat(MSG_RESPONSE);
		resp.WriteInt(reqIdx);

		trace(Form("Received request %i", reqIdx));
		
		processInstruction(req, resp);
		
		if (resp.Length() > 12) out.send(resp);
	}
}
