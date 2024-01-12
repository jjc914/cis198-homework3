use std::collections::HashMap;
use std::fmt;
use std::collections::VecDeque;

use regex::Regex;

use crate::graph::*;

pub struct Compiler {}

/* 
 * the lexer 
 */
#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum TokenType {
	Identifier,
	KeywordUnsignedInt8,
	KeywordReturn,
	KeywordIf,
	KeywordElse,

	OpenParenthesis,
	CloseParenthesis,
	OpenBrace,
	CloseBrace,
	IntLiteral,
	Semicolon,
	Minus,
	BitComplement,

	AdditionOperator,
	MultiplicationOperator,
	DivisionOperator,

	AssignmentOperator,

	Unknown,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::KeywordUnsignedInt8 => write!(f, "KeywordUnsignedInt8"),
            TokenType::KeywordReturn => write!(f, "KeywordReturn"),
            TokenType::KeywordIf => write!(f, "KeywordIf"),
            TokenType::KeywordElse => write!(f, "KeywordElse"),
            TokenType::OpenParenthesis => write!(f, "OpenParenthesis"),
            TokenType::CloseParenthesis => write!(f, "CloseParenthesis"),
            TokenType::OpenBrace => write!(f, "OpenBrace"),
            TokenType::CloseBrace => write!(f, "CloseBrace"),
            TokenType::IntLiteral => write!(f, "IntLiteral"),
            TokenType::Semicolon => write!(f, "Semicolon"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::BitComplement => write!(f, "BitComplement"),
            TokenType::AdditionOperator => write!(f, "AdditionOperator"),
            TokenType::MultiplicationOperator => write!(f, "MultiplicationOperator"),
            TokenType::DivisionOperator => write!(f, "DivisionOperator"),
            TokenType::AssignmentOperator => write!(f, "AssignmentOperator"),
            TokenType::Unknown => write!(f, "Unknown"),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Token {
	token_type: TokenType,
	content: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token(type: {}, content: \"{}\")", self.token_type, self.content)
    }
}

impl Compiler {
	fn get_token_type(token: &str, map: &HashMap<TokenType, Regex>) -> TokenType {
		/* the keyword library is here */
		let keyword_regex: HashMap<TokenType, Regex> = HashMap::from([
			(TokenType::KeywordUnsignedInt8,    Regex::new("ui8").unwrap()),
			(TokenType::KeywordReturn, 		    Regex::new("return").unwrap()),
		]);

		for (token_type, regex) in map.iter() {
			if regex.is_match(token) {
				match token_type {
					TokenType::Identifier => {
						for (keyword_type, keyword_regex) in keyword_regex.iter() {
							if keyword_regex.is_match(token) {
								return *keyword_type;
							}
						}
						return TokenType::Identifier;
					},
					_ => { 
						return *token_type;
					}
				}
			}
		}
		TokenType::Unknown
	}

	pub fn lex(program: String) -> VecDeque<Token> {
		// pass 1: identify words (symbols), and special characters. 

		/* the token library is here (special characters, etc) */
		let token_regex: HashMap<TokenType, Regex> = HashMap::from([
			(TokenType::Identifier, 				 Regex::new("^[a-zA-Z]\\w*$").unwrap()),
			(TokenType::OpenParenthesis, 			 Regex::new("\\(").unwrap()),
			(TokenType::CloseParenthesis, 			 Regex::new("\\)").unwrap()),
			(TokenType::OpenBrace,	 				 Regex::new("\\{").unwrap()),
			(TokenType::CloseBrace, 				 Regex::new("\\}").unwrap()),
			(TokenType::IntLiteral, 				 Regex::new("^\\d+$").unwrap()),
			(TokenType::Semicolon, 					 Regex::new(";").unwrap()),
			(TokenType::Minus,	 					 Regex::new("-").unwrap()),
			(TokenType::BitComplement, 				 Regex::new("~").unwrap()),

			(TokenType::AdditionOperator,			 Regex::new("\\+").unwrap()),
			(TokenType::MultiplicationOperator,		 Regex::new("\\*").unwrap()),
			(TokenType::DivisionOperator,			 Regex::new("/").unwrap()),

			(TokenType::AssignmentOperator,			 Regex::new("=").unwrap()),
		]);

		let master_regex = Regex::new("\\w+|[^\\s]").unwrap();

		// TODO: make an unidentified character regex

		// pass 2: identify keyworks and identifiers
		master_regex.find_iter(&program).map(|m| {
			let token_type = Self::get_token_type(m.as_str(), &token_regex);
			if token_type == TokenType::Unknown {
				panic!("unrecognized token {}", m.as_str());
			}
			Token {
				token_type: token_type, 
				content: String::from(m.as_str())
			}
		}).collect()
	}
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum ConstructType {
	Program,
	Function,
	Statement(StatementType),
	Assign(AssignType),
	Sum(ExpressionType),
	Multi(MultiType),
	Factor(FactorType),

	Type,
	Identifier,
	IntLiteral,
	Negate,
	BitComplement,
	LogicalNegate,

	Addition,
	Subtraction,
	Multiplication,
	Division,
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum StatementType {
	Declaration,
	Return,
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum AssignType {
	Assignment
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum ExpressionType {
	Addition,
	Subtraction,
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum MultiType {
	Multiplication,
	Division,
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum FactorType {
	Negate,
	BitComplement,
	LogicalNegate,
	IntLiteral,
	Variable,
}

impl fmt::Display for ConstructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstructType::Program => write!(f, "Program"),
            ConstructType::Function => write!(f, "Function"),
            ConstructType::Statement(statement_type) => write!(f, "Statement::{}", statement_type),
            ConstructType::Assign(assign_type) => write!(f, "Statement::{}", assign_type),
            ConstructType::Sum(product_type) => write!(f, "Sum::{}", product_type),
            ConstructType::Multi(multi_type) => write!(f, "Multi::{}", multi_type),
            ConstructType::Factor(factor_type) => write!(f, "Factor::{}", factor_type),
            ConstructType::Type => write!(f, "Type"),
            ConstructType::Identifier => write!(f, "Identifier"),
            ConstructType::IntLiteral => write!(f, "IntLiteral"),
            ConstructType::Negate => write!(f, "Negate"),
            ConstructType::BitComplement => write!(f, "BitComplement"),
            ConstructType::LogicalNegate => write!(f, "LogicalNegate"),
            ConstructType::Addition => write!(f, "Addition"),
            ConstructType::Subtraction => write!(f, "Subtraction"),
            ConstructType::Multiplication => write!(f, "Multiplication"),
            ConstructType::Division => write!(f, "Division"),
        }
    }
}

impl fmt::Display for StatementType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StatementType::Declaration => write!(f, "Declaration"),
            StatementType::Return => write!(f, "Return"),
        }
    }
}

impl fmt::Display for AssignType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignType::Assignment => write!(f, "Assignment"),
        }
    }
}

impl fmt::Display for ExpressionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionType::Addition => write!(f, "Addition"),
            ExpressionType::Subtraction => write!(f, "Subtraction"),
        }
    }
}

impl fmt::Display for MultiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MultiType::Multiplication => write!(f, "Multiplication"),
            MultiType::Division => write!(f, "Division"),
        }
    }
}

impl fmt::Display for FactorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FactorType::Negate => write!(f, "Negate"),
            FactorType::BitComplement => write!(f, "BitComplement"),
            FactorType::LogicalNegate => write!(f, "LogicalNegate"),
            FactorType::IntLiteral => write!(f, "IntLiteral"),
            FactorType::Variable => write!(f, "Variable"),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Construct {
	pub uid: usize,
	pub construct_type: ConstructType,
	pub content: String,
}

impl fmt::Display for Construct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Construct(uid: {}, type: {}, content: \"{}\")", self.uid, self.construct_type, self.content)
    }
}

/* 
 * the parser 
 */
impl Compiler {
	fn tokens_assert_next(tokens: &mut VecDeque<Token>, token_type: TokenType) {
		match tokens.pop_front() {
			Some(token) => {
				if token.token_type != token_type {
					panic!("expected {} but got {}", token_type, token.token_type);
				}
			},
			None => {
				panic!("expected {} but got nothing", token_type);
			}
		}
	}

	// TODO: add function to add new node in the tree from parent node. it should also figure out what number to give. 

	fn parse_program(tokens: &mut VecDeque<Token>, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		// <program> ::= <function>
		// programs are defined by functions:
		let program_construct = Construct{uid: *uid, construct_type: ConstructType::Program, content: "".to_string()};
		ast.push_vertex(program_construct.clone()).unwrap();
		*uid += 1;

		match tokens.get(0) {
			Some(token) => {
				match token.token_type {
					TokenType::KeywordUnsignedInt8 => {
						// found function (only int supported right now)
						Self::parse_function(tokens, &program_construct, ast, uid);
					},
					_ => {
						// no fu
						panic!("expected int function but got something else");
					}
				}
			},
			None => {
				panic!("expected function but got nothing");
			}
		}
	}

	fn parse_function(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
		// function-level parsing. 
		// after this function gets the tokens, it should be in format <id>, "(", ")", "{", (statement)
		Self::tokens_assert_next(tokens, TokenType::KeywordUnsignedInt8);

		let id = match tokens.pop_front() {
			Some(token) => {
				match token.token_type {
					TokenType::Identifier => {
						token.content
					},
					_ => {
						panic!("expected identifier after function type declaration but got something else");
					}
				}
			},
			None => {
				panic!("expected identifier after function type declaration but got nothing");
			}
		};
		Self::tokens_assert_next(tokens, TokenType::OpenParenthesis);
		Self::tokens_assert_next(tokens, TokenType::CloseParenthesis);
		Self::tokens_assert_next(tokens, TokenType::OpenBrace);

		// create function node
		let function_construct = Construct{uid: *uid, construct_type: ConstructType::Function, content: "".to_string()};
		ast.push_vertex(function_construct.clone()).unwrap();
		ast.push_edge(0, &parent_vertex, &function_construct).unwrap();
		*uid += 1;

		// store the id
		let id_construct = Construct{uid: *uid, construct_type: ConstructType::Identifier, content: id};
		ast.push_vertex(id_construct.clone()).unwrap();
		ast.push_edge(0, &function_construct, &id_construct).unwrap();
		*uid += 1;

		let mut edge = 1;
		while tokens[0].token_type != TokenType::CloseBrace {
			Self::parse_statement(tokens, &function_construct, ast, edge, uid);
			edge += 1;
		}

		Self::tokens_assert_next(tokens, TokenType::CloseBrace);
	}

	// TODO: require return statement
	fn parse_statement(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, edge: usize, uid: &mut usize) {
		// <statement> ::= "return" <sum> ";" | "ui8" <id> [ "=" <sum> ] ";"
		match tokens.get(0) {
			Some(token) => {
				match token.token_type {
					TokenType::KeywordReturn => {
						let token = tokens.pop_front().unwrap();
						// return statement
						let statement_construct = Construct{uid: *uid, construct_type: ConstructType::Statement(StatementType::Return), content: "".to_string()};
						let _ = ast.push_vertex(statement_construct.clone()).unwrap();
						ast.push_edge(edge, &parent_vertex, &statement_construct).unwrap();
						*uid += 1;

						Self::parse_expression(tokens, &statement_construct, ast, uid);
					},
					TokenType::KeywordUnsignedInt8 => {
						let token = tokens.pop_front().unwrap();
						// variable assignment
						let id = match tokens.pop_front() {
							Some(id_token) => {
								match id_token.token_type {
									TokenType::Identifier => {
										id_token.content
									},
									_ => {
										panic!("expected identifier after type but got something else");
									}
								}
							},
							None => {
								panic!("expected identifier after type but got nothing");
							}
						};
						Self::tokens_assert_next(tokens, TokenType::AssignmentOperator);

						let statement_construct = Construct{uid: *uid, construct_type: ConstructType::Statement(StatementType::Declaration), content: "".to_string()};
						let _ = ast.push_vertex(statement_construct.clone()).unwrap();
						ast.push_edge(edge, &parent_vertex, &statement_construct).unwrap();
						*uid += 1;

						let id_construct = Construct{uid: *uid, construct_type: ConstructType::Identifier, content: id};
						let _ = ast.push_vertex(id_construct.clone()).unwrap();
						ast.push_edge(1, &statement_construct, &id_construct).unwrap();
						*uid += 1;

						Self::parse_expression(tokens, &statement_construct, ast, uid);
					},
					_ => {
						Self::parse_expression(tokens, parent_vertex, ast, uid);
					},
				}
			},
			None => {
				panic!("expected statement but got nothing");
			}
		}

		Self::tokens_assert_next(tokens, TokenType::Semicolon);
	}

	fn parse_expression(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		Self::parse_assign(tokens, parent_vertex, ast, uid);
	}

	fn parse_assign(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		match tokens.get(1) {
			Some(token) => {
				if token.token_type == TokenType::AssignmentOperator && tokens.get(0).unwrap().token_type == TokenType::Identifier {
					// assigment statement
					let id = tokens.pop_front().unwrap().content;
					let assign_construct = Construct{uid: *uid, construct_type: ConstructType::Assign(AssignType::Assignment), content: "".to_string()};
					let _ = ast.push_vertex(assign_construct.clone()).unwrap();
					ast.push_edge(15, &parent_vertex, &assign_construct).unwrap();
					*uid += 1;

					let id_construct = Construct{uid: *uid, construct_type: ConstructType::Identifier, content: id.clone()};
					let _ = ast.push_vertex(id_construct.clone()).unwrap();
					ast.push_edge(1, &assign_construct, &id_construct).unwrap();
					*uid += 1;

					Self::tokens_assert_next(tokens, TokenType::AssignmentOperator);

					Self::parse_expression(tokens, &assign_construct, ast, uid);
				} else {
					Self::parse_product(tokens, parent_vertex, ast, uid);
				}
			},
			None => {
				// cannot be assignment, so just parse prod
				Self::parse_product(tokens, parent_vertex, ast, uid);
			}
		}
	}

	fn parse_product(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		/*
			<sum> ::= <multi> { ("+" | "-") <multi> } 
		*/

		Self::parse_multi(tokens, &parent_vertex, ast, uid);

		loop {
			let next = tokens.get(0).unwrap();
			let expr_type = match next.token_type {
				TokenType::AdditionOperator => ExpressionType::Addition,
				TokenType::Minus => ExpressionType::Subtraction,
				_ => {
					break;
				}
			};
			tokens.pop_front();

			let multi_vertex = ast.traverse_edge(&parent_vertex, &0).unwrap().clone();
			let _ = ast.remove_edge(&parent_vertex, &multi_vertex);

			let operator_construct = Construct{uid: *uid, construct_type: ConstructType::Sum(expr_type), content: "".to_string()};
			let _ = ast.push_vertex(operator_construct.clone()).unwrap();
			ast.push_edge(0, &parent_vertex, &operator_construct).unwrap();
			ast.push_edge(1, &operator_construct, &multi_vertex).unwrap();
			*uid += 1;

			Self::parse_multi(tokens, &operator_construct, ast, uid);
		}
	}

	fn parse_multi(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		// <multi> ::= <factor> { ("*" | "/") <factor> }
		Self::parse_factor(tokens, &parent_vertex, ast, uid);

		loop {
			let next = tokens.get(0).unwrap();
			let multi_type = match next.token_type {
				TokenType::MultiplicationOperator => MultiType::Multiplication,
				TokenType::DivisionOperator => MultiType::Division,
				_ => {
					break;
				}
			};
			tokens.pop_front();

			let factor_vertex = ast.traverse_edge(&parent_vertex, &0).unwrap().clone();
			let _ = ast.remove_edge(&parent_vertex, &factor_vertex);

			let operator_construct = Construct{uid: *uid, construct_type: ConstructType::Multi(multi_type), content: "".to_string()};
			let _ = ast.push_vertex(operator_construct.clone()).unwrap();
			ast.push_edge(0, &parent_vertex, &operator_construct).unwrap();
			ast.push_edge(1, &operator_construct, &factor_vertex).unwrap();
			*uid += 1;

			Self::parse_factor(tokens, &operator_construct, ast, uid);
		}
	}

	fn parse_factor(tokens: &mut VecDeque<Token>, parent_vertex: &Construct, ast: &mut Graph<Construct, usize>, uid: &mut usize) {
		// <factor> ::= (("~" | "-") <factor>) | <int>
		/*
			TODO: split factor into unary and int_literal
			 <factor> ::= (<unary> <factor>) | <int>
			 <unary> ::= "~" | "-"
			and in the ast, have unary and int literal be different nodes instead of both being factor
			also, split the enum into different enums to better segregate their types i.e. Factor(FactorType::Unary) or Factor(FactorType::IntLiteral)
		*/
		match tokens.pop_front() {
			Some(token) => {
				match token.token_type {
					TokenType::OpenParenthesis => {
						Self::parse_product(tokens, &parent_vertex, ast, uid);
						Self::tokens_assert_next(tokens, TokenType::CloseParenthesis);
					},
					TokenType::IntLiteral => {
						let literal_construct = Construct{uid: *uid, construct_type: ConstructType::Factor(FactorType::IntLiteral), content: token.content};
						let _ = ast.push_vertex(literal_construct.clone()).unwrap();
						ast.push_edge(0, &parent_vertex, &literal_construct).unwrap();
						*uid += 1;
					},
					TokenType::Minus => {
						let negate_construct = Construct{uid: *uid, construct_type: ConstructType::Factor(FactorType::Negate), content: "".to_string()};
						let _ = ast.push_vertex(negate_construct.clone()).unwrap();
						ast.push_edge(0, &parent_vertex, &negate_construct).unwrap();
						*uid += 1;

						Self::parse_factor(tokens, &negate_construct, ast, uid);
					},
					TokenType::BitComplement => {
						let complement_construct = Construct{uid: *uid, construct_type: ConstructType::Factor(FactorType::BitComplement), content: "".to_string()};
						let _ = ast.push_vertex(complement_construct.clone()).unwrap();
						ast.push_edge(0, &parent_vertex, &complement_construct).unwrap();
						*uid += 1;
						
						Self::parse_factor(tokens, &complement_construct, ast, uid);
					},
					TokenType::Identifier => {
						let variable_construct = Construct{uid: *uid, construct_type: ConstructType::Factor(FactorType::Variable), content: token.content};
						let _ = ast.push_vertex(variable_construct.clone()).unwrap();
						ast.push_edge(0, &parent_vertex, &variable_construct).unwrap();
						*uid += 1;
					},
					_ => {
						panic!("expected unary sum or integer literal within factor but got something else");
					}
				}
			},
			None => {
				panic!("expected unary sum or or identifier or integer literal within factor but got nothing");
			}
		}
	}

/*
 * 
 * <program> ::= <function>
 * <function> ::= "ui8" <id> "(" ")" "{" { <statement> } "}"
 * <statement> ::= "return" <assign> ";" |
				   "ui8" <id> [ "=" <assign> ] ";" |
				   <assign>
 * <assign> ::= <id> [ "=" <sum> ]
 * <sum> ::= <multi> { ("+" | "-") <multi> } 
 * <multi> ::= <sum> { ("*" | "/") <sum> }
 * <factor> ::= ("(" <sum> ")") | (("~" | "-") <factor>) | <int>
 * 
 */ 

	pub fn parse(mut tokens: VecDeque<Token>) -> Graph<Construct, usize> {
		let mut ast: Graph<Construct, usize> = Graph::new();
		let mut uid: usize = 0;

		Self::parse_program(&mut tokens, &mut ast, &mut uid);
		ast
	}
}

/* 
 * the generator 
 * IMPORTANT: all the assembly is for mac m1 arm archetecture right now
 */
impl Compiler {
	fn recursive_generate_postfix<'a>(node: &'a Construct, to_visit: &mut Vec<&'a Construct>, ast: &'a Graph<Construct, usize>) {
		// best not to do this recursively, but good enough for now. also the sorting is inefficient but im not gonna optimize it for now
		let mut leaves: Vec<(&usize, &Construct)> = ast.iter_targets(node).unwrap().collect();
		leaves.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

		for (_, leaf) in leaves {
			Self::recursive_generate_postfix(leaf, to_visit, ast);
		}
		to_visit.push(node);
	}

	fn generate_program(assembly: &mut String) {
		*assembly = String::from(format!(".global _main\n\
										  .align 16\n\
										  {assembly}"));
	}

	fn generate_function(assembly: &mut String) {
		*assembly = String::from(format!("_main:\n\
										  str x29, [sp]\n\
										  mov x29, sp\n\
									      {assembly}\n\
									      ldr x29, [sp]\n\
									      mov sp, x29\n\
									      ret"));
	}

	fn generate_return_statement(assembly: &mut String) {
		*assembly = String::from(format!("{assembly}\
										  ldr x0, [sp]\n\
								    	  add sp, sp, #16\n\
								    	  mov x16, #1\n\
				 				    	  svc 0"));
	}

	fn generate_decl_statement(assembly: &mut String, variable_offsets: &mut HashMap<String, usize>) {
		// assembly.push_str(&format!("mov x1, #0\n\
		// 						    sub sp, sp, #16\n\
		// 						    str x1, [sp]\n"));
	}

	fn generate_add(assembly: &mut String) {
		assembly.push_str("ldr x1, [sp]\n\
						   add sp, sp, #16\n\
						   ldr x2, [sp]\n\
						   add x1, x1, x2\n\
						   str x1, [sp]\n");
	}

	fn generate_subtract(assembly: &mut String) {
		assembly.push_str("ldr x1, [sp]\n\
						   add sp, sp, #16\n\
						   ldr x2, [sp]\n\
						   sub x1, x1, x2\n\
						   str x1, [sp]\n");
	}

	fn generate_multiply(assembly: &mut String) {
		assembly.push_str("ldr x1, [sp]\n\
						   add sp, sp, #16\n\
						   ldr x2, [sp]\n\
						   mul x1, x1, x2\n\
						   str x1, [sp]\n");
	}

	fn generate_divide(assembly: &mut String) {
		assembly.push_str("ldr x1, [sp]\n\
						   add sp, sp, #16\n\
						   ldr x2, [sp]\n\
						   udiv x1, x1, x2\n\
						   str x1, [sp]\n");
	}

	fn generate_negate(assembly: &mut String) {
		assembly.push_str("ldr x1, [sp]\n\
						   neg x1, x1\n\
						   str x1, [sp]\n");
	}

	fn generate_bit_complement(assembly: &mut String) {
		assembly.push_str("ldr x1, [sp]\n\
						   mvn x1, x1\n\
						   str x1, [sp]\n");
	}

	fn generate_int_literal(node: &Construct, assembly: &mut String) {
		let literal = &node.content;
		assembly.push_str(&format!("mov x1, #{literal}\n\
								    sub sp, sp, #16\n\
								    str x1, [sp]\n"));
	}

	fn generate_variable_access(node: &Construct, assembly: &mut String, variable_offsets: &HashMap<String, usize>) {
		let offset = match variable_offsets.get(&node.content) {
			Some(offset) => offset,
			None => {
				panic!("variable not declared");
			}
		};
		assembly.push_str(&format!("ldr x1, [x29, #-{offset}]\n\
								    sub sp, sp, #16\n\
								    str x1, [sp]\n"));
	}

	pub fn generate(ast: Graph<Construct, usize>) -> String {
		// find the root node
		let root = Construct{uid: 0, construct_type: ConstructType::Program, content: "".to_string()};

		let mut to_visit = Vec::new(); // the postfix vector

		Self::recursive_generate_postfix(&root, &mut to_visit, &ast);

		let mut assembly = String::new();
		let mut variable_offsets = HashMap::new();
		let mut stack_offset: usize = 0;

		for data in to_visit {
			match data.construct_type {
				ConstructType::Program => {
					Self::generate_program(&mut assembly);
				},
				ConstructType::Function => {
					Self::generate_function(&mut assembly);
				},
				ConstructType::Statement(StatementType::Return) => {
					Self::generate_return_statement(&mut assembly);
				},
				ConstructType::Statement(StatementType::Declaration) => {
					let id = ast.traverse_edge(&data, &1).unwrap().content.clone();

					Self::generate_decl_statement(&mut assembly, &mut variable_offsets);

					variable_offsets.insert(id, stack_offset);
				},
				ConstructType::Identifier => {
					continue;
				},
				ConstructType::Sum(ExpressionType::Addition) => {
					Self::generate_add(&mut assembly);
					stack_offset -= 16;
				},
				ConstructType::Sum(ExpressionType::Subtraction) => {
					Self::generate_subtract(&mut assembly);
					stack_offset -= 16;
				},
				ConstructType::Multi(MultiType::Multiplication) => {
					Self::generate_multiply(&mut assembly);
					stack_offset -= 16;
				},
				ConstructType::Multi(MultiType::Division) => {
					Self::generate_divide(&mut assembly);
					stack_offset -= 16;
				},
				ConstructType::Factor(FactorType::Negate) => {
					Self::generate_negate(&mut assembly);
				},
				ConstructType::Factor(FactorType::BitComplement) => {
					Self::generate_bit_complement(&mut assembly);
				},
				ConstructType::Factor(FactorType::IntLiteral) => {
					Self::generate_int_literal(data, &mut assembly);
					stack_offset += 16;
				},
				ConstructType::Factor(FactorType::Variable) => {
					Self::generate_variable_access(data, &mut assembly, &variable_offsets);
					stack_offset += 16;
				},
				_ => {
					todo!();
				}
			}
		}
		assembly
	}
}