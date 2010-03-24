
#ifndef max
#define max(a,b) ((a)>(b) ? (a) : (b))
#endif

#ifndef min
#define min(a,b) ((a)<(b) ? (a) : (b))
#endif

/* Prototypes !!! */

void merge(void);
void screen_on(void);
void screen_off(void);
void shuffle(void);
void fatal_error(char *);
short bin_compare(unsigned char *,unsigned char *);
short try_to_win(struct board *);
void free_bin_tree(struct bintree *);
short evaluation_function(struct board *);
short test_dbtree_position(struct dbtree *,unsigned char *);
short heuristic_search(struct node *,long);
long readkey(void);
short resources_available(void);
struct dbtree *init_dbase(unsigned char *,short);
void copy_dbtree(void);
void update_dbtree(struct bintree *,struct dbtree *,short);
void flush_dbtree(struct dbtree *);
void flush_tree(struct dbtree *,FILE *);
void free_dbtree(struct dbtree *);
void add_dbtree(struct dbtree *,unsigned char *,short);
short makemove(struct board *,short);
short undomove(struct board *,short);
short endgame(struct board *);
short ia_compute_move(struct board *);
void dropscreen(struct board *);
void clear_instances(void);
void initscreen(void);
void whomoves(struct board *);
void dumpboard(struct board *);
short start_pn_search(struct board *);
void debug_function(struct board *);
void update_log(struct board *);
short heuristic_play_best(struct board *,long);
char **allocate_matrix(struct board *);
void free_matrix(char **,struct board *);
void build_adjacency_matrix(char **,struct board *);
void dump_no_adjacencies(char **,struct board *);
short problem_solver(struct board *,char **,short,FILE *);
void change_sequence(void);
int my_random(unsigned short);
short check_book(struct board *,unsigned char *,short);
short check_presence(struct board *,short);
void outtextxy(int,int,char *);
int process_mouse_board(struct board *,int,int);
int process_mouse_menu(int,int);

unsigned long pseudo32(unsigned long *);

long fileln(FILE *);

