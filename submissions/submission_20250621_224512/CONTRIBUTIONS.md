# Contributions
Ognjen, Florian, Nicolas

### Setup and Infra & Pipeline
- Nico: Providing Github Repo to work on things + setup. Managing pull requests, updating tests from organizers etc.
- Pull Requests & Updates to Code by all of us.

### Task 1 - Contributions
#### Task 1.1: (Leading Zeros Counter)
- Implementation: Ognjen
- Review and Input: Nicolas & Florian
- Challenges and Insights: Nothing - Just looking in the manual.

#### Task 1.2: GREV(I)
- Implementation: Nicolas 
- Review and Input: Ognjen & Florian
- Challenges and Insights: Base Case was initially bad, reduced from 2 bitwidth to 1. Manual gave it away.

#### Task 1.3: (UN-SHFLI)(I)
- Implementation:  Florian 
- Review and Input: Ognjen, Nicolas
- Challenges and Insights: Manual made it straigh-forward

#### Task 1.4:
- Implementation: Nicolas (Seq-Rotater) & Ognjen (Fixed Rotater)
- Review and Input: Florian
- Challenges and Insights: Setting the io.done and io.result in the right order in the right states of the circuit. state Pattern with Idle, Active and Done helped alot to have a systematic way of checking if io.done and io.result were set correctly.


### Task 2 - Contributions
#### Task 2.1: *Insert John Travolta Meme here*
- Implementation: Ognjen
- Review and Input: Nicolas & Florian
- Challenges and Insights:

#### Task 2.2: Decoder, EU, CU instructions implementations
- Implementation: Ognjen
- Review and Input: Nicolas & Florian
- Challenges and Insights: Decoder: had more inputs than the required for implementation, forum post cleared it up. EU was relatively straightforward.

#### Task 2.3: Decoder, EU, CU instructions implementations
- Implementation: Ognjen
- Review and Input: Nicolas & Florian
- Challenges and Insights: Control Unit: Selection of Registers was already implemented in the first when, we didn't catch that initially and didn't make use of it (for the load instructions) and overwrote it initially. Later adjusted it.

#### Task 2.4: Bit Manipulation Instructions
- Implementation: Florian
- Review and Input: Nicolas & Ognjen
- Challenges and Insights: Personal difference between us and Scala. Also: Confused functions with classes (lowercase class names - yikes...). Let's just continue.

#### Task 2.5 Bit Permutation Instructions
- Implementation: Nicolas
- Review and Input: Ognjen & Florian
- Challenges and Insights: Actually worked with wrong decodings for a while being completely flabbergasted about why instructions sometimes worked or not depending on how I set reg_rs2 with 0.U or our rs2 value. Florian fixed this. After that the code worked perfectly fine and the Mux to set reg_rs2 worked as expected.

#### Task 2.6: Generating sequence of instructions for perm pseudo-instruction.
- Implementation: Nicolas
- Debugging & Optimization + Review: Basically Nico but also Ognjen and Florian
- Challenges & Insights: Coming up with an algorithm that was actually a bit smart about generating the right sequences relatively fast. Also: Choice and Order of tried-out immediates (which could influence search time big time).

# Use of AI tools
Did you use any AI tools for the project? If so, which tools did you use?

- We used AI for explanations of high-level concepts and to confirm some assumptions we had about interaction between register file, ALU etc.
- We used AI for Debugging, sometimes helpful, sometimes not.
- Used AI to give ideas about approaches to the Bonus task, it gave us Brutce Force, which we then implemented into a Breadth First Search (BFS) and later adapted to Bidirectional BFS with Pruning and pre-check for common patterns.

## Problem 2.6: Bonus
- If I don't find a solution, I return rori x1 x1 32 instruction, which is the identity instruction so as to not return an empty list.
- Used a mixed approach of detecting clear pattern quickly (not exhaustive!) and transitioned from there to an algorithm group I like and know: BFS and DFS (also used in 2 of my Programming 2 projects). Permutations are traversal locations to reach or to cross over and find the target permutation.
- Initially, I implemented a straight-forward BFS, which uses my instructions and immediates and tries them out: I considered BFS to be more reliable than DFS, especially since we had to find short solutions, directly going into deep instruction sequences of DFS felt counter-intuitive. (That is not to say that maybe DFS is actually much better for finding permutations that have longer solutions). It tries out several immediates before moving onto a new instruction. If all are executed, I add a new "layer" of instructions and try out again.
- Restricted my BFS search to depth 4 because with depth 5 it consistently timed out and my C++ script got "Killed.". (I did O3 optimization). I then realized that using bidirectional approach to the BFS can effectively double the depth level of my instruction solutions by starting the searching from the initial permutation and target permutation, which also allowed me to finding sequences > 4 but sadly due to system ressources constraints only max 8 instructions. This is a big limitation in my code! When I get lucky with depth = 5, I can even get solutions up to length 10. But for the most part, it is halted due to memory exhaustion.
- Forward and Backward allowed me to basically lookup a permutation after it had been transposed by an instruction and check, whether it was already visited. If it was, I knew that the forward search and the backwards search instructions (reversed and inversed!) lead to the target permutation. The "meet in the middle" idea came from the RoutePlanner Java project that I did last year, where for Bonus points we had to implement Bidirectional Path Finding.
- However, the solutions that my C++ script provided, needed to be wrapped with grevi x1 x1 31 and sadly, I couldn't trace back
and find a way to adjust the input in my scala implementation such that it passed the tests there. I only got the idea with grevi after writing more tests and realizing that some of the outputs were somehow exactly mirrored index manipulations from my original target permutation. For example: Instead of 1-4 swap : 27-30 swap.
- Specifics of the BFS: I have visited set to avoid instruction orders that lead to reoccuring permutations. Also: I check whether the newly chosen instruction is not the exact inverse of the previous instruction, because if it is it would not contribute to the search.
- In the end, I got to know another algorithm which was supposedly doing this a really good way but I didn't have time anymore to implement it. So, I am leaving it out.

- Team and I reviewed the code but we couldn't detect the reason for the weird behaviour in the code. AI (Claude) also didn't.

- Edit on 02.07.2025: I wrote a scala function that mirrors my changes in the target permutation along the middle and now it gives me the correct instruction sequences without having to wrap them with grevi 31 instructions => Best solutions are now 2 less instructions long. See `CustomPermutation.scala` under `/src/main/scala/RISC-V/utils`
- Edit on 03.07.2025: Removed getMirroredPermutation because real culprit found with rori in .scala and .cpp

- See my C++ implementation script with custom hashing for breaking up patterns: `perm_finder.cpp` under `/submission`


### One final notice:
- 02.07.2025: This approach allows me now to compute instruction sequences for swap with distance 1 to 4.
- For example: adjacent swaps and swaps like 21 <=> 25 etc. One more note: This is even better than only knowing the instruction sequence solutions for adjacent swaps.
- Easy solutions but not elegant: As mentioned, I do have all the instruction sequences for doing adjacent swaps. However, giving a solution of chained-together swap instruction sequences to finally reach an arbitrary permutation feels a bit stupid and
not the intention of this exercise or how you intended it.
- Thus, I am not providing such solutions. Especially, seeing the bonus points calculation, I assume that those solutions
would be so far away from the best solutions in cycles and amount of instructions that I think, I'd not get points for
those anyways. BUt main thing is: I do not want to provide 'stupid' solutions.
Being aware of different approaches like Hamming Distance etc., I really wanted to explore with the limited time
how I can come up with my own solutions.
- That being sad: Knowing all solutions for swaps with distance 1 to 4, one can calculate the minimal number of swaps with 1-4 distance and accodring to this build a more efficient chain of instruction sequences. The only remaining thing: Have an intelligent approach that can condense this chain into less instructions via identity checks, immediate-inverse-followup instruction elimination as well as maximum interval deletion for reoccuring transposition results along the chain of instruction sequences. (Maybe a TODO for free time)
- Interesting approaches:
    1. Hamming Distances or Other Distance metrics.
    2. Mask metrics / similarity checks given what our 4 allowed instructions do and how similar a permutation seems to be to a mask (mainly specific arithmetics)
    3. Some Concurrent Programming Models 
    4. Optimization of long generated instruction sequences by checking all generated permutations until target permutation is reached => Deleting those instructions inbetween EXCLUDING the instruction that first lead to this permutation. The search would have started from the front and the back in a Dictionary: Key-Value = Instruction_i:Permutation_i. This would effectively remove the longest sequence of instructions that lead to a reoccuring permutation, effectively deleting the corresponding sequence of instructions inbetween, potentially heavily optimizing "trial and error" solutions.
    5. Getting rid of wrapping grevi x1 x1 31: In my python script, I first reversed the list, then executed the C++ generated instructions and then reversed the list again - e vòila: The right target permutation, however, I did not know how to fix this in code.
    6. Really loved this bonus task in the project.