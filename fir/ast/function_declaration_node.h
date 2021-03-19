#ifndef __FIR_AST_FUNCTION_DECLARATION_H__
#define __FIR_AST_FUNCTION_DECLARATION_H__

#include <string>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>

namespace fir {

    class function_declaration_node: public cdk::typed_node {
        int _qualifier;
        std::string _identifier;
        cdk::sequence_node *_arguments;
        cdk::expression_node *_returnvalue;

        public:
        function_declaration_node(int lineno, int qualifier, const std::string &identifier, cdk::sequence_node *arguments, cdk::expression_node *returnvalue = nullptr) :
                cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _arguments(arguments), _returnvalue(returnvalue) {
            type(cdk::primitive_type::create(0, cdk::TYPE_VOID));
        }

        function_declaration_node(int lineno, int qualifier, std::shared_ptr<cdk::basic_type> funType, const std::string &identifier,
                                cdk::sequence_node *arguments, cdk::expression_node *returnvalue = nullptr) :
                cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _arguments(arguments), _returnvalue(returnvalue) {
            type(funType);
        }

        public:
        int qualifier() {
            return _qualifier;
        }

        const std::string& identifier() const {
            return _identifier;
        }

        cdk::typed_node* argument(size_t ax) {
            return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
        }

        cdk::sequence_node* arguments() {
            return _arguments;
        }
        
        cdk::expression_node* returnvalue() {
            return _returnvalue;
        }

        void accept(basic_ast_visitor *sp, int level) {
            sp->do_function_declaration_node(this, level);
        }

    };

} // fir

#endif
