package esmeta.wji.interpreter

import esmeta.wji.bridge.host.{HostFunction, WasmError, WasmHost}
import esmeta.wji.state.ALValue

/** A WasmHost that logs every call to stdout and returns a stub error.
  *
  * Used to drive the interpreter without a live SpecTec process, so we can
  * observe which embedding operations would be invoked and at what point
  * execution halts.
  */
class StubWasmHost extends WasmHost:

  private def stub(name: String, args: Any*): Either[WasmError, ALValue] =
    val argStr = args.map(_.toString).mkString(", ")
    println(s"  [stub] $name($argStr)")
    Left(WasmError.ProtocolError(s"stub: $name not implemented"))

  def storeInit() = stub("store_init")

  def moduleDecode(bytes: ALValue) = stub("module_decode", bytes)
  def moduleValidate(module: ALValue) = stub("module_validate", module)
  def moduleInstantiate(
    store: ALValue,
    module: ALValue,
    externVals: List[ALValue],
  ) =
    stub("module_instantiate", store, module, externVals)
  def moduleImports(module: ALValue) = stub("module_imports", module)
  def moduleExports(module: ALValue) = stub("module_exports", module)

  def instanceExport(moduleInst: ALValue, name: ALValue) =
    stub("instance_export", moduleInst, name)

  def funcAlloc(store: ALValue, defType: ALValue, hostFunc: HostFunction) =
    stub("func_alloc", store, defType)
  def funcType(store: ALValue, funcAddr: ALValue) =
    stub("func_type", store, funcAddr)
  def funcInvoke(store: ALValue, funcAddr: ALValue, args: List[ALValue]) =
    stub("func_invoke", store, funcAddr, args)

  def tableAlloc(store: ALValue, tableType: ALValue, ref: ALValue) =
    stub("table_alloc", store, tableType, ref)
  def tableType(store: ALValue, tableAddr: ALValue) =
    stub("table_type", store, tableAddr)
  def tableRead(store: ALValue, tableAddr: ALValue, i: ALValue) =
    stub("table_read", store, tableAddr, i)
  def tableWrite(store: ALValue, tableAddr: ALValue, i: ALValue, ref: ALValue) =
    stub("table_write", store, tableAddr, i, ref)
  def tableSize(store: ALValue, tableAddr: ALValue) =
    stub("table_size", store, tableAddr)
  def tableGrow(store: ALValue, tableAddr: ALValue, n: ALValue, ref: ALValue) =
    stub("table_grow", store, tableAddr, n, ref)

  def memAlloc(store: ALValue, memType: ALValue) =
    stub("mem_alloc", store, memType)
  def memType(store: ALValue, memAddr: ALValue) =
    stub("mem_type", store, memAddr)
  def memSize(store: ALValue, memAddr: ALValue) =
    stub("mem_size", store, memAddr)
  def memGrow(store: ALValue, memAddr: ALValue, n: ALValue) =
    stub("mem_grow", store, memAddr, n)

  def tagAlloc(store: ALValue, tagType: ALValue) =
    stub("tag_alloc", store, tagType)
  def tagType(store: ALValue, tagAddr: ALValue) =
    stub("tag_type", store, tagAddr)

  def exnAlloc(store: ALValue, tagAddr: ALValue, vals: List[ALValue]) =
    stub("exn_alloc", store, tagAddr, vals)
  def exnTag(store: ALValue, exnAddr: ALValue) = stub("exn_tag", store, exnAddr)
  def exnRead(store: ALValue, exnAddr: ALValue) =
    stub("exn_read", store, exnAddr)

  def globalAlloc(store: ALValue, globalType: ALValue, value: ALValue) =
    stub("global_alloc", store, globalType, value)
  def globalType(store: ALValue, globalAddr: ALValue) =
    stub("global_type", store, globalAddr)
  def globalRead(store: ALValue, globalAddr: ALValue) =
    stub("global_read", store, globalAddr)
  def globalWrite(store: ALValue, globalAddr: ALValue, value: ALValue) =
    stub("global_write", store, globalAddr, value)

  def refType(store: ALValue, ref: ALValue) = stub("ref_type", store, ref)
  def valDefault(valType: ALValue) = stub("val_default", valType)

  def matchValType(valType1: ALValue, valType2: ALValue) =
    stub("match_valtype", valType1, valType2)
  def matchExternType(externType1: ALValue, externType2: ALValue) =
    stub("match_externtype", externType1, externType2)
