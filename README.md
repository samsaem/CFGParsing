# Transition-based Parsing
This project explores how different parsing strategies handle sentence structures that are **left-branching**, **right-branching**, or **center-embedded**, aiming to shed light on how these methods might reflect human language processing.

### Examples

**Left-branching:**
- Mary won  
- Mary’s baby won  
- Mary’s boss’s baby won  

**Right-branching:**
- John met the boy  
- John met the boy that saw the actor  
- John met the boy that saw the actor that won the award  

**Center-embedding:**
- The actor won  
- The actor the boy met won  
- The actor the boy the baby saw met won  

### Parsing and Stack Behavior

By analyzing how stack size evolves in different parsing approaches, we can draw analogies to how the human brain might process language. It's often suggested that humans use a kind of internal stack-like mechanism during sentence comprehension.

Humans tend to handle left- and right-branching sentences with relative ease, while more complex center-embedded sentences are notably harder to process.

Parsing method behaviors:

- **Bottom-up parsing**  
  - Stack stays constant for **left-branching**  
  - Grows for **right-branching** and **center-embedding**

- **Top-down parsing**  
  - Stack stays constant for **right-branching**  
  - Grows for **left-branching** and **center-embedding**

- **Left-corner parsing**  
  - Stack stays constant for **left- and right-branching**  
  - Grows only for **center-embedding**

### Conclusion

Among the parsing strategies, **left-corner parsing** appears to align most closely with the limits and efficiencies observed in human sentence processing.


## Personal Code Analysis
### Bottom-Up Parsing
Bottom-Up (BU) parser constructs strings from the bottom of a tree, and includes two types of transitions, shift and match. At Marr’s computational level, the goal of this parser is within the name of the parser, “Bottom-Up”, which is deriving the input string from left to right, and starting from the bottom, working our way up the hierarchy to get to the root, where the grammar’s start symbols is located. BU uses the shift and transition algorithm to determine the route. For example, the starting configuration for BU is an empty stack, along with the input string. BU begins parsing by shifting the input starting from the left; if there is a rule that matches with the input, it will return the Terminal symbol on top of the stack, which is written on the right of the stack. Once there are enough of terminal symbols that can be “reduced”, BU will run the transition algorithm and return the left hand side Nonterminal symbol of that same rule onto the stack. Throughout the whole process, the stack will increase in size due to transition adding more grammar onto the configuration, and string will be decreasing with a decrement of one whenever shift consumes the input. 

At an implementation level, I will first start off with the shift algorithm. Since we are “shifting” the input onto the stack, I pattern match the input with base case being empty to denote empty input string at the final configuration; and utilize the helper function to rhsTRule to get the terminal symbol `x` from the rule. Since we only want the lhs symbol to be put on our stack, I “extract” the input by using a filter function on x, and put concat the new lhs symbol to the stack, to ensure that it is on top of the stack.

I utilize a similar solving approach to shift algorithm to solve reduce function. However, I find that it is more challenging to solve this and run into a minor problem. The current symbol/s on the stack tells us about the rhs of a rule, I then check to see if the rhs of the rule is the same as the lhs. Since our goal is to return only lhs grammar, I use the drop predefined function to drop the rhs of the rule, then connect the lhs to the stack. 
The below sentences were used to test the functionality of Bottom-Up parser, in which it correctly behaves the way that it should, except the placement of the added symbol onto the stack is incorrectly placed.
 
> Left-branching: Mary’s baby won
> 
> Right-branching: the baby saw the boy
> 
> Center-embedding: The actor the boy the baby saw met won

```
ghci> shift [(TRule D "the")] ([NoBar D, NoBar N, NoBar D, NoBar N], ["the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([D,N,D,N,D],["baby","saw","met","won"])
===== END PARSE =====
ghci> shift [(TRule N "baby")] ([NoBar D, NoBar N, NoBar D, NoBar N, NoBar D], ["baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Shift   N -> "baby"   ([D,N,D,N,D,N],["saw","met","won"])
** should be [D, N, D, N, NP] instead of [NP, D, N, D, N]
ghci> reduce [(NTRule NP [D,N])] ([NoBar D, NoBar N, NoBar D, NoBar N, NoBar D, NoBar N], ["saw", "met", "won"])
===== BEGIN PARSE =====
Reduce   NP -> D N   ([NP,D,N,D,N],["saw","met","won"])
===== END PARSE =====
```

Initially, no bugs were found when I first tested the reduce function. As I continue testing, I realized that the reduce function works well if there is a small number of symbols that need to be reduced on stack, and it stills work on bigger stack, but with incorrect placing. As provided in the rule, the top of the stack is written on the right. However, the result from my function is written on the left. I tried fixing the placement by changing up the position of stack and lhs rule, creating new variable to store the new “reduced” Nonterminal, etc, but I was not able to achieve the result that I needed.

In conclusion, Bottom-Up shifts the input and searches for the correct rule that can be applied with the input to push its lhs Terminal symbol onto the stack, then determines whether or not it can apply reduction based on its current rhs symbols on the configuration.

### Top-down Parsing
In contrast to the Bottom-Up parser, Top-Down (TD) begins at the root and works its way down. At Marr’s computational level, the goal of TD parser is that, a start grammar symbol at the root and the input is given, the parser needs to solve the structures by predicting the rules based on the information that has been given, put the “predicted” rules on stack, and match the input when appropriate. We know parsing is completed when both the stack and the input is empty. Algorithmically, Top-Down uses the predict and match algorithm to make predictions based on the stack history. If there is a matching rule with the symbols on the stack, predict will put the predictions onto the stack, and compare those predictions with the given inputs. If there is any pair that can be matched between the input and stacks, the match algorithm will match it together. TD will repeat this process until both the size of stack and input is empty. If there is no progress that can further happen, Top-Down parser either backtracks to find the solution or fails entirely, which outputs our base case. 

Based on the given stack at the starting configuration, I construct the predict function by casing the stack, then pattern matching the Nonterminal symbols, which is x, on the stack with the base case being empty. If x is found on the lhs rule, then we map its right hand side to the stack, then recursively apply it to the rest. Moreover, x is guaranteed to not be included on the stack with the predefined filter function. Move on to the implementation of the match function, which I find pretty challenging to solve because now we are looking to solve both the stack, and the input altogether. The rule of match transition is that, if we have a matching pair of (stack, input) in the configuration, then we match it by looking for the correct rule then remove both pairs from the stack. 

Similarly to the base case of predict, match’s base case is also empty. I decided to case stack first and assign x as the NoBar nt symbols, then case the input in order to apply the predefined rules to get the TRule and the NTRule. I find this function challenging because of two case expressions within the function and it was taking some time to figure out which is what in order to apply the correct variable to match with the Terminal/Nonterminal symbols. Once that is sorted out, I map the rule and combine the results into the list to get the new configuration.

The Top-Down parser works correctly with all three types of embedding structures and I used the three sentences below to test for the stack depth:

> Left-branching: the baby saw the boy
> 
> Right-branching: John met the boy that saw the actor that won the award
> 
> Center-embedding: the actor the boy the baby saw met won

```
ghci> predict [(NTRule NP [D,N,ORC])] ([NoBar NP, NoBar V, NoBar VP], ["the", "boy", "the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Predict   NP -> D N ORC   ([D,N,ORC,V,VP],["the","boy","the","baby","saw","met","won"])
===== END PARSE =====
ghci> predict [(NTRule NP [D,N])] ([NoBar NP, NoBar V, NoBar V, NoBar VP], ["the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Predict   NP -> D N   ([D,N,V,V,VP],["the","baby","saw","met","won"])
===== END PARSE =====
```

As it turns out, the stack depth results from TD test cases match with our chart from Homework 7. Moreover, Top-Down parser definitely takes more toll on the memory size as it was demonstrated from the provided example. Since it is making predictions for the structures, the parser will keep adding new symbols onto the stack and won’t free up memory until there is a match. 

To conclude, predict looks at the current symbol on top of the stack and expands with any possible ways that it can, match transition verifies if the current input can go with the predictions and removes from the configuration if it can.

### Left-corner Parsing
Left-corner (LC) parsing is a combination of top-down and bottom-up. Similar to Top-Down parsing, the starting configuration of Left-Corner includes the starting A-bar symbol, and the input string with the goal being not having any stack, or input string on the configuration. There is also a difference between the Bar symbols and the NoBar symbols for Left-Corner. Since LC utilizes the transitions from both Top-Down and Bottom-Up, it uses the Bar symbols to denote when the parser is behaving in a top-down manner, and NoBar symbols to denote when it is behaving in the bottom-up manner.

At the computational level, Left-Corner works through the configuration by implementing a hybrid system, taking advantage of the transitions from two other parsers, and the goal being connecting all of the input symbols with its grammar rules. LC utilizes the shift from BU and match, predict from TD. It also has another transition “connect” that behaves similarly to its predict transition.

Due to Left-Corner borrowing the transitions from other parsers, I implemented these transitions similarly to how I did and only do minor adjustments to return the correct Bar and NonBar symbols. To start off with the shiftLC function, it is the same as the shift function that I implemented previously from Bottom-Up. Next one is predictLC, which I implemented differently from the TD’s predict due to the Bar and NoBar symbol, as well as the return value. Since we are making predictions based on the previous stack history, I search for the left-corner of the rule by pattern matching it with the rhs of a rule. Parser then attempts to expand the stack by “predicting” the correct right hand side rules, and recognizes its left hand side. 

Since predictLC correctly predicts the rhs, it will return the grammar symbols with a Bar notation because of the predicting behavior from a top-down manner, and return the lhs NoBar Nonterminal in a bottom-up manner, which are both defined as a new variable then concat together with rhs Bar symbols as the first element and NoBar appends after Bar. Similar to how match was constructed, I implemented matchLC the same but with an adjustment to x being the Bar symbol. The reason for this is simply to denote that matchLC behaves in a top-down manner. Last but not least, the connectLC transition, which I have a hard time constructing and not able to correctly implement to output a result that I needed. I attempted to pattern match the Bar nt symbol to check if there exists a rule that can connect it with the lhs, then attach the remainder of the rhs rule onto the stack. Unfortunately after multiple tries, I cannot get connectLC to work.

Due to only being able to solve ¾ transition functions for the Left-Corner parser, I am only 75% sure that my parser correctly identifies all of the possible parses and the embedding structures. I used these sentences below to compare the stack depth to Homework 7 chart:

>Left-branching: the baby saw the boy
>
>Right-branching: the actor the boy the baby saw met won

```
ghci> shiftLC ([TRule D "the"]) ([Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["the", "baby", "saw"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([D,ORC*,NP,ORC*,NP,S*],["baby","saw"])
===== END PARSE =====
ghci> predictLC [(NTRule NP [D,N])] ([NoBar D, Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["baby", "saw"])
===== BEGIN PARSE =====
Predict   NP -> D N   ([N*,NP,ORC*,NP,ORC*,NP,S*],["baby","saw"])
===== END PARSE =====
ghci> matchLC [(TRule N "baby")] ([Bar N, NoBar NP, Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["baby", "saw"])
===== BEGIN PARSE =====
Match   N -> "baby"   ([NP,ORC*,NP,ORC*,NP,S*],["saw"])
===== END PARSE =====
```

>Center-embedding: the actor the boy the baby saw met won

Since connectLC's goal is to reduce the size of the stack and not increase, I am confident that the result of my stack depth from the Left-Corner parser is the same as the Homework 7 chart.

In conclusion, LC implements a hybrid system between Top-Down and Bottom-Up for its parsing process. It begins by looking at the left-corner symbol to predict the correct rule, and go through different transitions with the goal being connecting all of the Bar and NoBar symbols to reach the final configuration with both stack and input string being empty. 
