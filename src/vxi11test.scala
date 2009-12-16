import org.acplt.oncrpc.OncRpcProtocols
import java.net.InetAddress
import org.daqcore.vxi11rpc._

val address = InetAddress.getByName("gpib03")
val client = new CoreClient(address, OncRpcProtocols.ONCRPC_TCP)

val defaultTimeout:Int = 10000

val lparms = new Create_LinkParms
lparms.lockDevice = false
lparms.lock_timeout = defaultTimeout
lparms.device = "gpib0,22"
val lresp = client.create_link_1(lparms)

println("create_link error: " + lresp.error.value)

val link = lresp.lid

lresp.maxRecvSize
// Steve D. Sharples:
// We need to check that maxRecvSize is a sane value (ie >0). Believe it
// or not, on some versions of Agilent Infiniium scope firmware the scope
// returned "0", which breaks Rule B.6.3 of the VXI-11 protocol. Nevertheless
// we need to catch this, otherwise the program just hangs.

// Use 4096 as safe default for maxRecvSize if it reads 0
       
val wparms = new Device_WriteParms
wparms.lid = link
wparms.io_timeout = defaultTimeout
wparms.lock_timeout = defaultTimeout
wparms.flags = new Device_Flags(8); // set to 0 if not last data chunk
val data: Array[Byte] = "*IDN?\r\n".getBytes()
wparms.data = data

//!!! If data size > maxRecvSize, write seveal chunks!

val wresp = client.device_write_1(wparms)
// If write rpc call fails (!= RPC_SUCCESS) - retry or fail?
// Steve D. Sharples:
// The instrument did not acknowledge the write, just completely
// dropped it. There was no vxi11 comms error as such, the 
// instrument is just being rude. Usually occurs when the instrument
// is busy.

println("device_write error: " + wresp.error.value)
println("bytes written: " + wresp.size) // bytes written
//!!! if wresp.size < data size, write rest

val rparms = new Device_ReadParms
rparms.lid = link
rparms.io_timeout = defaultTimeout
rparms.lock_timeout = defaultTimeout
rparms.requestSize = 1024
rparms.flags = new Device_Flags(0)
rparms.termChar = 0

val rresp = client.device_read_1(rparms)
// If read rpc call fails (!= RPC_SUCCESS) - retry or fail?
// Steve D. Sharples:
// there is nothing to read. Usually occurs after sending a query
// which times out on the instrument. If we don't check this first,
// then the following line causes a seg fault

println("device_read error: " + rresp.error.value)

//!!! If error value != 0:
// Steve D. Sharples:
// Read failed for reason specified in error code.
//   *  0     no error
//   *  4     invalid link identifier
//   *  11    device locked by another link
//   *  15    I/O timeout
//   *  17    I/O error
//   *  23    abort

val rcv_reason_end:Int = 0x04; // An end indicator has been read
val rcv_reason_chr:Int = 0x02; // A termchr is set in flags and a character which matches termChar is transferred
val rcv_reason_reqcnt:Int = 0x01; // requestSize bytes have been transferred.  This includes a request size of zero.

println("device_read reason: " + rresp.reason)

//!!! if end or chr bit set, read is complete, if not, more chunks to read
//    (possible reasons: requestSize too small, or message broken into chunks
//    by instrument)

val response = new String(rresp.data)

println("read result: " + response)

val unlinkResp = client.destroy_link_1(link)

println("destroy_link error: " + unlinkResp.error.value)

client.close()
