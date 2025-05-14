// From d.hyun.ahn snitch
package klase32.include

import chisel3._
import chisel3.util._
import klase32.include.config._
import chisel3.util.log2Ceil
import klase32.include.param.KLASE32ParamKey


object param {
  case class KLASE32CoreDIVParam(
                                useClz: Boolean = true
                                )
  case class KlasE32CoreParam(
                             usingMulDiv: Boolean = true,
                             usingAtomics: Boolean = false,
                             fLen: Int = 0,
                             usingVector: Boolean = false,
                             usingCompressed: Boolean = true,
                             usingRVE: Boolean = false,
                             customIsaExt: Option[String] = None,
                             usingSupervisor: Boolean = false,
                             usingHypervisor: Boolean = false,
                             usingUser: Boolean = false,
                             usingOuterBootAddr: Boolean = true,
                             mpyLatency: Int = 2,
                             bootAddr: BigInt = BigInt(0x0008_0000L),
                             // for debugger
                             debugAddr: BigInt = BigInt(0x6000_0800L),
                             debugExceptionAddr: BigInt = BigInt(0x6000_0808L),

                             mxLen: Int = 32,
                             sxLen: Int = 32,
                             uxLen: Int = 32,

                             readportNum: Int = 2,
                             writeportNum: Int = 1,

                             outstandingLoad: Int = 2,
                             fetchQueueEntries: Int = 3,
                             useInstKill: Boolean = false,
                             loadstorequeueEntries: Int = 4,
                             storeBufferEntries: Int = 0,

                             causeWidth: Int = 4,
                             hartIDWidth: Int = 4,

                             addressAlignByte: Int = 4,

                             issueWidth: Int = 1,

                             divParam: KLASE32CoreDIVParam = KLASE32CoreDIVParam(
                               useClz = true
                             )
                             )
  case class KlasE32Param(
                           vaddrBits: Int = 32,
                           paddrBits: Int = 32,
                           dataWidth: Int = 32,
                           fetchWidth: Int = 1,
//                           dataWidth: Int = 64,
//                           fetchWidth: Int = 2,
                           accdataWidth: Int = 32,
                           accidWidth: Int = 32,
                           tbminterfacenum: Int = 4,
                           core: KlasE32CoreParam = KlasE32CoreParam(),
                         ) {
    val fetchBits: Int = fetchWidth * 32
    def dataAlign = log2Ceil(dataWidth/8)
  }

  case object KLASE32ParamKey extends Field[KlasE32Param](KlasE32Param())

  class DefaultConfig extends Config((site, here, up) => {
    case KLASE32ParamKey => KlasE32Param()
  })
}

object KLASE32AbstractClass {
  trait HasCoreParameters {
    implicit val p: Parameters

    def coreParams = p(KLASE32ParamKey).core

    val wordsize: Int = 32

    def usingMulDiv = coreParams.usingMulDiv

    def usingAtomics = coreParams.usingAtomics

    def fLen = coreParams.fLen

    def usingVector = coreParams.usingVector

    def usingCompressed = coreParams.usingCompressed

    def usingRVE = coreParams.usingRVE

    def customIsaExt = coreParams.customIsaExt

    def usingSupervisor = coreParams.usingSupervisor

    def usingHypervisor = coreParams.usingHypervisor

    def usingUser = coreParams.usingUser

    def usingOuterBootAddr = coreParams.usingOuterBootAddr

    def bootAddrParam = coreParams.bootAddr

    def debugAddrParam = coreParams.debugAddr

    def debugExceptionAddrParam = coreParams.debugExceptionAddr

    def regNum = if (usingRVE) 16 else 32

    def regIdWidth = log2Ceil(regNum)

    def readportNum = coreParams.readportNum

    def writeportNum = coreParams.writeportNum

    def fetchQueueEntries = coreParams.fetchQueueEntries

    def useInstKill: Boolean = coreParams.useInstKill

    def loadstorequeueEntries = coreParams.loadstorequeueEntries

    def storeBufferEntries = coreParams.storeBufferEntries

    def mxLen = coreParams.mxLen

    def sxLen = coreParams.sxLen

    def uxLen = coreParams.uxLen

    def csrWidthM = coreParams.mxLen

    def csrWidthS = coreParams.sxLen

    def csrWidthU = coreParams.uxLen

    def xLen = Seq(mxLen, sxLen, uxLen).max

    def causeWidth = coreParams.causeWidth

    def hartIDWidth = coreParams.hartIDWidth

    def addressAlignByte = coreParams.addressAlignByte

    def issueWidth = coreParams.issueWidth

    def issueBits = issueWidth * 32

    def mpyLatency = coreParams.mpyLatency
    def divParam = coreParams.divParam
  }

  abstract class CoreModule(implicit val p: Parameters) extends Module with HasCoreParameters

  abstract class CoreBundle(implicit val p: Parameters) extends Bundle with HasCoreParameters
}


object Constants {
  object Privilege {
    val MMode = 3.U
    val UMode = 0.U
    val SMode = 1.U
  }

  object DebugType {
    val MControl6 = 6.U
    val ICount = 3.U
  }

  object ActionType {
    val Breakpoint = 0.U
    val DMode = 1.U
  }

  object DebugCause {
    val GroupHartReq = 6.U
    val ResetHartReq = 5.U
    val SingleStep = 4.U
    val HartReq = 3.U
    val Trigger = 2.U
    val EBreak = 1.U
  }
}
