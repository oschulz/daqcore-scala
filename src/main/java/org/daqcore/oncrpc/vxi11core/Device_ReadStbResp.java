/*
 * Automatically generated by jrpcgen 1.0.7 on 22.12.09 14:59
 * jrpcgen is part of the "Remote Tea" ONC/RPC package for Java
 * See http://remotetea.sourceforge.net for details
 */
package org.daqcore.oncrpc.vxi11core;
import org.acplt.oncrpc.*;
import java.io.IOException;

public class Device_ReadStbResp implements XdrAble, java.io.Serializable {
    public Device_ErrorCode error;
    public byte stb;

    private static final long serialVersionUID = 6084576921838896777L;

    public Device_ReadStbResp() {
    }

    public Device_ReadStbResp(XdrDecodingStream xdr)
           throws OncRpcException, IOException {
        xdrDecode(xdr);
    }

    public void xdrEncode(XdrEncodingStream xdr)
           throws OncRpcException, IOException {
        error.xdrEncode(xdr);
        xdr.xdrEncodeByte(stb);
    }

    public void xdrDecode(XdrDecodingStream xdr)
           throws OncRpcException, IOException {
        error = new Device_ErrorCode(xdr);
        stb = xdr.xdrDecodeByte();
    }

}
// End of Device_ReadStbResp.java
