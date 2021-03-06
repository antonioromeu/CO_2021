#ifndef __FIR_AST_FOR_NODE_H__
#define __FIR_AST_FOR_NODE_H__

#include <cdk/ast/expression_node.h>

namespace fir {

  /**
   * Class for describing for-cycle nodes.
   */
  class for_node: public cdk::basic_node {
    cdk::expression_node *_init;
    cdk::expression_node *_condition;
    cdk::expression_node *_incr;
    cdk::basic_node *_instr;

  public:
    inline for_node(int lineno, cdk::expression_node *init, cdk::expression_node *condition, 
        cdk::expression_node *incr, cdk::basic_node *instr) : basic_node(lineno), 
        _init(init), _condition(condition), _incr(incr), _instr(instr) {
    }

  public:
    inline cdk::expression_node *init() {
      return _init;
    }
    inline cdk::expression_node *condition() {
      return _condition;
    }
    inline cdk::expression_node *incr() {
      return _incr;
    }
    inline cdk::basic_node *instr() {
      return _instr;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_for_node(this, level);
    }

  };

} // fir

#endif
