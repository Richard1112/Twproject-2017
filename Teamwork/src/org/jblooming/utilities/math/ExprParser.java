package org.jblooming.utilities.math;// $ANTLR 2.7.6 (20051207): "math.g" -> "ExprParser.java"$

import antlr.*;
import antlr.collections.AST;
import antlr.collections.impl.BitSet;

public class ExprParser extends antlr.LLkParser       implements ExprParserTokenTypes
 {

protected ExprParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
  buildTokenTypeASTClassMap();
  astFactory = new ASTFactory(getTokenTypeToASTClassMap());
}

public ExprParser(TokenBuffer tokenBuf) {
  this(tokenBuf,1);
}

protected ExprParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
  buildTokenTypeASTClassMap();
  astFactory = new ASTFactory(getTokenTypeToASTClassMap());
}

public ExprParser(TokenStream lexer) {
  this(lexer,1);
}

public ExprParser(ParserSharedInputState state) {
  super(state,1);
  tokenNames = _tokenNames;
  buildTokenTypeASTClassMap();
  astFactory = new ASTFactory(getTokenTypeToASTClassMap());
}

	public final double  expr() throws RecognitionException, TokenStreamException {
		double value=0;

		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST expr_AST = null;
		double x;

		try {      // for error handling
			value=mexpr();
			astFactory.addASTChild(currentAST, returnAST);
			{
			_loop3:
			do {
				switch ( LA(1)) {
				case PLUS:
				{
					AST tmp1_AST = null;
					tmp1_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp1_AST);
					match(PLUS);
					x=mexpr();
					astFactory.addASTChild(currentAST, returnAST);
					value += x;
					break;
				}
				case MINUS:
				{
					AST tmp2_AST = null;
					tmp2_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp2_AST);
					match(MINUS);
					x=mexpr();
					astFactory.addASTChild(currentAST, returnAST);
					value -= x;
					break;
				}
				default:
				{
					break _loop3;
				}
				}
			} while (true);
			}
			expr_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_0);
		}
		returnAST = expr_AST;
		return value;
	}

	public final double  mexpr() throws RecognitionException, TokenStreamException {
		double value=0;

		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST mexpr_AST = null;
		double x;

		try {      // for error handling
			value=atom();
			astFactory.addASTChild(currentAST, returnAST);
			{
			_loop6:
			do {
				switch ( LA(1)) {
				case STAR:
				{
					AST tmp3_AST = null;
					tmp3_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp3_AST);
					match(STAR);
					x=atom();
					astFactory.addASTChild(currentAST, returnAST);
					value *= x;
					break;
				}
				case POWER:
				{
					AST tmp4_AST = null;
					tmp4_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp4_AST);
					match(POWER);
					x=atom();
					astFactory.addASTChild(currentAST, returnAST);
					value = (Math.pow(value,x));
					break;
				}
				case DIV:
				{
					AST tmp5_AST = null;
					tmp5_AST = astFactory.create(LT(1));
					astFactory.addASTChild(currentAST, tmp5_AST);
					match(DIV);
					x=atom();
					astFactory.addASTChild(currentAST, returnAST);
					value = (value/x);
					break;
				}
				default:
				{
					break _loop6;
				}
				}
			} while (true);
			}
			mexpr_AST = (AST)currentAST.root;
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_1);
		}
		returnAST = mexpr_AST;
		return value;
	}

	public final double  atom() throws RecognitionException, TokenStreamException {
		double value=0;

		returnAST = null;
		ASTPair currentAST = new ASTPair();
		AST atom_AST = null;
		Token  i = null;
		AST i_AST = null;
    double x;

    try {      // for error handling
			switch ( LA(1)) {
			case NUM:
			{
				i = LT(1);
				i_AST = astFactory.create(i);
				astFactory.addASTChild(currentAST, i_AST);
				match(NUM);
				value=Double.parseDouble(i.getText());
				atom_AST = (AST)currentAST.root;
				break;
			}
			case LPAREN:
			{
				AST tmp6_AST = null;
				tmp6_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp6_AST);
				match(LPAREN);
				value=expr();
				astFactory.addASTChild(currentAST, returnAST);
				AST tmp7_AST = null;
				tmp7_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp7_AST);
				match(RPAREN);
				atom_AST = (AST)currentAST.root;
				break;
			}
			case MINUS:
			{
				AST tmp8_AST = null;
				tmp8_AST = astFactory.create(LT(1));
				astFactory.addASTChild(currentAST, tmp8_AST);
				match(MINUS);
				x=mexpr();
				astFactory.addASTChild(currentAST, returnAST);
				value=-x;
				atom_AST = (AST)currentAST.root;
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_2);
		}
		returnAST = atom_AST;
		return value;
	}


	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"PLUS",
		"MINUS",
		"STAR",
		"POWER",
		"DIV",
		"NUM",
		"LPAREN",
		"RPAREN",
		"WS"
	};

	protected void buildTokenTypeASTClassMap() {
		tokenTypeToASTClassMap=null;
	}

	private static final long[] mk_tokenSet_0() {
		long[] data = { 2048L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { 2096L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	private static final long[] mk_tokenSet_2() {
		long[] data = { 2544L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_2 = new BitSet(mk_tokenSet_2());

	}
