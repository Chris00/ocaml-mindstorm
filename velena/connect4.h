
/*#define DEBUG*/

#define LOGFILE     "connect4.log"
#define TMPFILENAME "tempfile.$$$"

#define TEMP_BOOK  "openbook.cn4"

#define WHITE_BOOK "white_ob.cn4"
#define BLACK_BOOK "black_ob.cn4"

#define ARROW_UP   0x4800
#define ARROW_DOWN 0x5000

#define PAGE_UP    0x4900
#define PAGE_DOWN  0x5100

#define TRUE  1
#define FALSE 0
#define YES   1
#define NO    0
#define ON    1
#define OFF   0

#define FULL      -1
#define EMPTY 		 0
#define WHITE 		 1
#define BLACK 		 2
#define SWITCHSIDE 3

#define WIN     2
#define DRAW    1
#define NOTHING 0

#define BOARDX 7
#define BOARDY 6

#define MAXSQUARES (BOARDX*BOARDY)

#define GROUPS     69
#define MAXSOLS   700
#define MAXGROUPS  50
#define TILES       4

#define MAXMEN     42

#define ELM(x,y) ((x)+((y)<<3))

#define ELX(n) ((n)&7)
#define ELY(n) ((n)>>3)

#define BOTH(x,y) ((x)+((y)<<6))

#define ALLOC_SOLUTIONS (9*GROUPS)

#define CHARS  0
#define VESAGR 1

#define SQ_a1   ELM(0,0)
#define SQ_a2   ELM(0,1)
#define SQ_a3   ELM(0,2)
#define SQ_a4   ELM(0,3)
#define SQ_a5   ELM(0,4)
#define SQ_a6   ELM(0,5)

#define SQ_b1   ELM(1,0)
#define SQ_b2   ELM(1,1)
#define SQ_b3   ELM(1,2)
#define SQ_b4   ELM(1,3)
#define SQ_b5   ELM(1,4)
#define SQ_b6   ELM(1,5)

#define SQ_c1   ELM(2,0)
#define SQ_c2   ELM(2,1)
#define SQ_c3   ELM(2,2)
#define SQ_c4   ELM(2,3)
#define SQ_c5   ELM(2,4)
#define SQ_c6   ELM(2,5)

#define SQ_d1   ELM(3,0)
#define SQ_d2   ELM(3,1)
#define SQ_d3   ELM(3,2)
#define SQ_d4   ELM(3,3)
#define SQ_d5   ELM(3,4)
#define SQ_d6   ELM(3,5)

#define SQ_e1   ELM(4,0)
#define SQ_e2   ELM(4,1)
#define SQ_e3   ELM(4,2)
#define SQ_e4   ELM(4,3)
#define SQ_e5   ELM(4,4)
#define SQ_e6   ELM(4,5)

#define SQ_f1   ELM(5,0)
#define SQ_f2   ELM(5,1)
#define SQ_f3   ELM(5,2)
#define SQ_f4   ELM(5,3)
#define SQ_f5   ELM(5,4)
#define SQ_f6   ELM(5,5)

#define SQ_g1   ELM(6,0)
#define SQ_g2   ELM(6,1)
#define SQ_g3   ELM(6,2)
#define SQ_g4   ELM(6,3)
#define SQ_g5   ELM(6,4)
#define SQ_g6   ELM(6,5)


struct solvable_groups {
							  short square[64][16];
							  short sqpnt[64];
							  };

struct solution {
					 short valid;
					 short solname;
					 short solpoint[2];
					 short sqinv[2*TILES];
					 short sqinvnumb;
					 short solgroups[GROUPS];
					 short solgroupsnumb;
					 };

struct intgp {
				 short tgroups[GROUPS];
				 short j,k;
				 short mygroups[GROUPS];
				 };

struct board {
				 short square[(BOARDX+1)*(BOARDY+2)];
				 short wipesq[(BOARDX+1)*(BOARDY+2)];
				 short usablegroup[GROUPS];
				 short sqused[(BOARDX+1)*(BOARDY+2)];
				 short stack[BOARDX+1];
				 short *groups[GROUPS][TILES];
				 short xplace[GROUPS][TILES];
				 short yplace[GROUPS][TILES];
                 short turn;
                 short moves[MAXMEN],choices[MAXMEN],mlist[MAXMEN];
				 short filled;

				 struct intgp intgp;
				 struct solution *solution[ALLOC_SOLUTIONS];
				 short sp;
				 short problem_solved,solused;
				 short oracle[2],oracle_guesses,lastguess,bestguess;
				 long nodes_visited;
				 short maxtreedepth;

				 unsigned long rule[3];

				 unsigned long instances[10];

				 short wins[2],draws,lastwin;
				 struct solvable_groups *solvable_groups;

                 unsigned char *white_book,*black_book;
                 long wbposit,bbposit;
                 long lastob;
                 short autotest,cpu,white_lev,black_lev;
                 short videotype,usegraphics,enablegr;
                 short debug;
                 };

