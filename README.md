# WORDSEARCH
 Fortran Program to Create ‘Find the Word’ Puzzle. more details at [here](https://sukhbinder.wordpress.com/2013/11/07/fortran-program-to-create-find-the-word-puzzle/)



#  How to run?

1. Download the program ``wordsearch.exe`` from ``dist`` folder
2. In the command line: ``wordsearch.exe  work outrageous pumpkin audible love like good `` 

```bash

 R M S I W C G C W W U R L W
 P U E W G D U W O D W E W P
 P S Y Y M G K N N G L A R L
 E P C M N C D R I N P Y K Y
 O F D O N E A D I D C B K S
 P F S O R O I T D M N I E U
 L U Y F A E I P T K W A M O
 E L U T K B I G D N G S A E
 T L A R Y P E L B I D U A G
 A C C E L E R A T E O I K A
 W P A R T I C I P A T E R R
 F S U O I R T S Y M C C O T
 O T R U I O G T A Y F S W U
 O U K Y E C M A F U T O U O


WONDERFUL     OUTRAGEOUS    MYSTRIOUS     PARTICIPATE   AUDIBLE       
ACCELERATE    CAT           SONG          PEOPLE        WORK          

```

To get the words in a file, use

```bash
``wordsearch.exe  work outrageous pumpkin audible love like good > words.txt`` 

```
This will produce the words in the file **words.txt**

# How to Build from source?

If you have gfortran

```bash
C:\>gfortran wordsearch.f90 -o dist/wordsearch.exe

```

To get gfortran and other compliers. Use

```python
conda install mingw
```

# Limitations

- Only 14 by 14 grids
- Words less than 13 letters support as of now 
