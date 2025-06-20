# Contributions
Ognjen Stojicic (7006292), Florian Philippi (7055169), Nicolas Dornblüth (7013576)

### Setup and Infra & Pipeline
- Nico: Providing Github Repo to work on things + setup. Managing pull requests, updating tests from organizers etc.
- Pull Requests & Updates to Code by all of us.

### Task 1 - Contributions
Task 1.1: (Leading Zeros Counter)
    - Implementation: Ognjen
    - Review and Input: Nicolas & Florian
    - Challenges and Insights: Nothing - Just looking in the manual.

Task 1.2: GREV(I)
    - Implementation: Nicolas 
    - Review and Input: Ognjen & Florian
    - Challenges and Insights: Base Case was initially bad, reduced from 2 bitwidth to 1. Manual gave it away.

Task 1.3: (UN-SHFLI)(I)
    - Implementation:  Florian 
    - Review and Input: Ognjen, Nicolas
    - Challenges and Insights: Manual made it straigh-forward

Task 1.4:
    - Implementation: Nicolas (Seq-Rotater) & Ognjen (Fixed Rotater)
    - Review and Input: Florian
    - Challenges and Insights: Setting the io.done and io.result in the right order in the right states of the circuit. state Pattern with Idle, Active and Done helped alot to have a systematic way of checking if io.done and io.result were set correctly.


### Task 2 - Contributions
Task 2.1: *Insert John Travolta Meme here*
    - Implementation: Ognjen
    - Review and Input: Nicolas & Florian
    - Challenges and Insights:

Task 2.2: Decoder, EU, CU instructions implementations
    - Implementation: Ognjen
    - Review and Input: Nicolas & Florian
    - Challenges and Insights: Decoder: had more inputs than the required for implementation, forum post cleared it up. EU was relatively straightforward.

Task 2.3: Decoder, EU, CU instructions implementations
    - Implementation: Ognjen
    - Review and Input: Nicolas & Florian
    - Challenges and Insights: Control Unit: Selection of Registers was already implemented in the first when, we didn't catch that initially and didn't make use of it (for the load instructions) and overwrote it initially. Later adjusted it.

Task 2.4: Bit Manipulation Instructions
    - Implementation: Florian
    - Review and Input: Nicolas & Ognjen
    - Challenges and Insights: Personal difference between us and Scala. Also: Confused functions with classes (lowercase class names - yikes...). Let's just continue.

Task 2.5 
    - Implementation: Nicolas 
    - Review and Input: Ognjen & Florian
    - Challenges and Insights: Actually worked with wrong decodings for a while being completely flabbergasted about why instructions sometimes worked or not depending on how I set reg_rs2 with 0.U or our rs2 value. Florian fixed this. After that the code worked perfectly fine and the Mux to set reg_rs2 worked as expected. 

Task 2.6: Generating sequence of instructions for perm pseudo-instruction.
    - Implementation: Nicolas
    - Debugging & Optimization + Review: Ognjen and Florian
    - Challenges & Insights: Coming up with an algorithm that was actually a bit smart about generating the right sequences relatively fast. Also: Choice and Order of tried-out immediates (which could influence search time big time).

# Use of AI tools
Did you use any AI tools for the project? If so, which tools did you use?

- We used AI for explanations of high-level concepts and to confirm some assumptions we had about interaction between register file, ALU etc.
- We used AI for Debugging, sometimes helpful, sometimes not.
- Used AI to give ideas about approaches to the Bonus task, it gave us Brutce Force, which we then implemented into a Breadth First Search (BFS) and later adapted to Bidirectional BFS with Pruning and pre-check for common pattern.

## Problem 2.6: Bonus
TODO