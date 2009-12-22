/*
 * Automatically generated by jrpcgen 1.0.7 on 22.12.09 14:59
 * jrpcgen is part of the "Remote Tea" ONC/RPC package for Java
 * See http://remotetea.sourceforge.net for details
 */
package org.daqcore.oncrpc.vxi11core;
import org.acplt.oncrpc.*;
import java.io.IOException;

public class Device_DocmdParms implements XdrAble, java.io.Serializable {
    public Device_Link lid;
    public Device_Flags flags;
    public int io_timeout;
    public int lock_timeout;
    public int cmd;
    public boolean network_order;
    public int datasize;
    public byte [] data_in;

    private static final long serialVersionUID = 4472709096566524486L;

    public Device_DocmdParms() {
    }

    public Device_DocmdParms(XdrDecodingStream xdr)
           throws OncRpcException, IOException {
        xdrDecode(xdr);
    }

    public void xdrEncode(XdrEncodingStream xdr)
           throws OncRpcException, IOException {
        lid.xdrEncode(xdr);
        flags.xdrEncode(xdr);
        xdr.xdrEncodeInt(io_timeout);
        xdr.xdrEncodeInt(lock_timeout);
        xdr.xdrEncodeInt(cmd);
        xdr.xdrEncodeBoolean(network_order);
        xdr.xdrEncodeInt(datasize);
        xdr.xdrEncodeDynamicOpaque(data_in);
    }

    public void xdrDecode(XdrDecodingStream xdr)
           throws OncRpcException, IOException {
        lid = new Device_Link(xdr);
        flags = new Device_Flags(xdr);
        io_timeout = xdr.xdrDecodeInt();
        lock_timeout = xdr.xdrDecodeInt();
        cmd = xdr.xdrDecodeInt();
        network_order = xdr.xdrDecodeBoolean();
        datasize = xdr.xdrDecodeInt();
        data_in = xdr.xdrDecodeDynamicOpaque();
    }

}
// End of Device_DocmdParms.java
