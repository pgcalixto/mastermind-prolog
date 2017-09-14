/*
 * Evaluator for Master Mind
 * J. Meidanis, 2017
 *
 * Evaluator for MasterMind programs.
 * Starts a swipl script as a child process and communicates with it.
 * Redirects the child's stderr to a file.
 * (Fork & redirection part based on code by Eike Rathke, 2001)
 * Selects a pattern, answers guesses, and produces a log.
 * Also evaluates the quality of the player, and stops players that
 * don't make progress fast enough.
 */

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>
#include <time.h>
#include <limits.h>

void usage(char *me) {
  fprintf(stderr, "Usage: %s <swipl-script> [<word>]\n\n", me);
  fprintf(stderr, "<swipl-script>: SWI Prolog script to be run\n");
  fprintf(stderr, "<word>: (optional) password to be guessed\n");
  fprintf(stderr, "(If no <word> is given, a random one is generated)\n");
}

void randomPattern(char *word, int nposs, int nword) {
  // Generate a random pattern
  // of size nword
  // from the numbers 1 to nposs, without repetition
  // nposs must be larger than nword
  srandom(time(NULL));
  char possible[nposs];
  int i;
  for (i = 0; i<nposs; i++) {
    possible[i] = '1'+i;
  }
  int size = nposs;
  for (int i = 0; i<nword; i++) {
    int k = random() % size;
    word[i] = possible[k];
    size--;
    possible[k] = possible[size];
  }
}

void score (char *guess, char *word, int nword, int *bons, int *regs) {
  *bons = 0;
  for (int i = 0; i < nword; i++ ) {
    if (guess[i] == word[i])
      (*bons)++;
  }
  *regs = 0;
  for (int i = 0; i < nword; i++ ) {
    for (int j = 0; j < nword; j++ ) {
      if ( i != j && guess[i] == word[j] )
	(*regs)++;
    }
  }
}

int main(int argc, char *argv[])
{
  pid_t nPid;
  int answer[2];      /* pipe to feed the exec'ed program input */
  int guess[2];    /* pipe to get the exec'ed program output */
  char *progName;  /* name of script to be tested */
  FILE *childStderr; 		/* to redirect child's stderr  */
  FILE *logFile; 		/* to print log  */


  if (argc >= 2) {
    progName = argv[1];
  } else {
    usage(argv[0]);
    exit(-1);
  }

  // log file
  logFile = fopen("log", "w");
  if ( logFile == NULL ) {
    perror( "fopen() log" );
    exit(-1);
  }

  // if given, second argument is word
  // if second arg not given or too short, word is chosen randomly
  int NPOSS = 6;
  int NWORD = 4;
  char word[NWORD];
  if (argc >= 3 && strlen(argv[2]) >= NWORD) {
    strncpy(word, argv[2], NWORD);
  } else {
    randomPattern(word, NPOSS, NWORD);
  }
  printf(" word: %4.4s\n", word);
  fprintf(logFile, " word: %4.4s\n", word);

  // create communication channels between paernt and child
  if ( pipe( answer ) != 0 ) {
    perror( "pipe() answer" );
    exit(-1);
  }

  if ( pipe( guess ) != 0 ) {
    perror( "pipe() guess" );
    exit(-1);
  }

  // create stream to redirect child stderr
  childStderr = fopen("child.stderr", "w");
  if ( childStderr == NULL ) {
    perror( "fopen() child stderr" );
    exit(-1);
  }

  // fork child process
  nPid = fork();
  if ( nPid < 0 ) {
    perror( "fork()" );
    exit(-1);
  } else if ( nPid == 0 ) {
    //========================================
    //===== child
    //===== PROGRAMA DO(A) ALUNO(A)
    /* dup pipe read/write to stdin/stdout */
    dup2( answer[0], STDIN_FILENO );
    dup2( guess[1], STDOUT_FILENO );
    dup2( fileno(childStderr), STDERR_FILENO );
    /* close unnecessary pipe descriptors for a clean environment */
    //close( answer[0] );
    //close( answer[1] );
    //close( guess[0] );
    //close( guess[1] );
    fclose(childStderr);
    /* call user's program */
    if (execlp( progName, NULL ) == -1) {
      perror( "execlp()" );
      exit(-1);
    }
  } else {
    //========================================
    //===== parent
    //===== PROGRAMA AVALIADOR
    FILE *input, *output;
    /* Close unused pipe ends. */
    // close( answer[0] );
    // close( guess[1] );

    // **************************************************
    // *** Preparation
    //printf("Pid: %d\n", nPid);

    if ( (input = fdopen( guess[0], "r" )) == NULL ) {
      perror( "fdopen() guess" );
      exit(-1);
    }

    if ( (output = fdopen( answer[1], "w" )) == NULL ) {
      perror( "fdopen() answer" );
      exit(-1);
    }
    // unbuffered output
    setvbuf(output, NULL, _IONBF, 0);

    // **************************************************
    // *** GAME

    char chute[NWORD];
    char buf[10000];
    // regular expression to parse guess
    // must have NWORD groups
    char *regex = " *\\[ *([1-6]) *, *([1-6]) *, *([1-6]) *, *([1-6]) *\\] *";
    regex_t regexCompiled;
    regmatch_t groupArray[NWORD+1];
    if ( regcomp(&regexCompiled, regex, REG_EXTENDED | REG_NEWLINE) ) {
      perror( "regcomp()" );
      exit(-1);
    }

    do {
      // Read, parse, score, and answer
      // Read
      fgets(buf, 1000, input);
      printf("chute: %s", buf);
      fprintf(logFile, "chute: %s", buf);
      // Parse
      if (regexec(&regexCompiled, buf, NWORD+1, groupArray, 0)) {
	if ((strcmp(buf, "ganhei\n") != 0) &&
	    (strcmp(buf, "erro\n") != 0)) {
	  printf("formato errado\n");
	}
	break;
      }
      for (int g = 0; g < NWORD; g++) {
	chute[g] = buf[groupArray[g+1].rm_so];
	for (int h = 0; h < g; h++) {
	  // verifies if there are repated numbers
	  if (chute[h] == chute[g]) {
	    chute[0] = '0';
	    break;
	  }
	}
	if (chute[0] == '0') {
	  break;
	}
      }
      if (chute[0] == '0') {
	printf("números repetidos\n");
	break;
      }
      //printf("chute: %4.4s\n", chute);
      // Score
      int bons, regs;
      score(chute, word, NWORD, &bons, &regs);
      // Answer
      printf("resp.: %d%d\n", bons, regs);
      fprintf(logFile, "resp.: %d%d\n", bons, regs);
      fprintf(output, "[%d,%d].\n", bons, regs);
    } while (1);
  }

  fflush( stdout );
  return( 0 );
}
