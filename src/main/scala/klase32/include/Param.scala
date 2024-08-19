// From d.hyun.ahn snitch
package klase32

import chisel3._
import chisel3.util._
import klase32.config._
import chisel3.util.log2Ceil
import klase32.param.KLASE32ParamKey


object param {
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

                               usingOuterBootAddr: Boolean = false,

                               mpyLatency: Int = 2,

                               bootAddr: BigInt = BigInt(0x80000000L),

                               mxLen: Int = 32,
                               sxLen: Int = 32,
                               uxLen: Int = 32,

                               regNum: Int = 32,
                               readportNum: Int = 2,
                               writeportNum: Int = 2,

                               outstandingLoad: Int = 2,
                               fetchqueueEntries: Int = 4,
                               useInstKill: Boolean = false,
//                               fetchqueueEntries: Int = 2,
                               loadstorequeueEntries: Int = 4,
                               storeBufferEntries: Int = 2,
//                               storeBufferEntries: Int = 4,

                               causeWidth: Int = 4,
                               hartIDWidth: Int = 4,

                               addressAlignByte: Int = 4,

                               issueWidth: Int = 1
                             )
  case class KlasE32Param(
                           vaddrBits: Int = 32,
                           paddrBits: Int = 32,
                           dataWidth: Int = 32,
                           fetchWidth: Int = 1,
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
  def regNum = coreParams.regNum
  def regIdWidth = log2Ceil(regNum)
  def readportNum = coreParams.readportNum
  def writeportNum = coreParams.writeportNum
  def fetchqueueEntries = coreParams.fetchqueueEntries
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

}

abstract class CoreModule(implicit val p: Parameters) extends Module with HasCoreParameters

abstract class CoreBundle(implicit val p: Parameters) extends Bundle with HasCoreParameters
