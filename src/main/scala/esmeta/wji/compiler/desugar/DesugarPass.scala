package esmeta.wji.compiler.desugar

import esmeta.wji.lang.Algorithm

trait DesugarPass:
  def run(algos: List[Algorithm]): List[Algorithm]
