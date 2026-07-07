package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

trait LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm]
