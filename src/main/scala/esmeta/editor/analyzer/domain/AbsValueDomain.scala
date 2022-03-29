package esmeta.editor.analyzer

trait AbsValueDomain extends Domain {

  type Elem <: AValueTrait

  trait AValueTrait extends ElemTrait { this: Elem =>
    def containsBool(b: Boolean): Boolean
  }
}
