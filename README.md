# Homework 6 (20 Points)

The deadline for Homework 6 is Tuesday, March 19, 6pm. The late
submission deadline is Saturday, March 23, 6pm.

## Getting the code template

Before you perform the next steps, you first need to create your own
private copy of this git repository. To do so, click on the link
provided in the announcement of this homework assignment on
Piazza. After clicking on the link, you will receive an email from
GitHub, when your copy of the repository is ready. It will be
available at
`https://github.com/nyu-pl-sp19/hw06-<YOUR-GITHUB-USERNAME>`.  
Note that this may take a few minutes.

* Open a browser at `https://github.com/nyu-pl-sp19/hw06-<YOUR-GITHUB-USERNAME>` with your Github username inserted at the appropriate place in the URL.
* Choose a place on your computer for your homework assignments to reside and open a terminal to that location.
* Execute the following git command: <br/>
  ```git clone https://github.com/nyu-pl-sp19/hw06-<YOUR-GITHUB-USERNAME>.git```<br/>

The code template is provided in the file

```
src/hw06.ml
```

Simply replace all occurrences of `failwith "Not yet implemented"` by
your implementation of the corresponding function.

There are also some unit tests in

```
src/hw06_spec.ml
```

to help you test your code.

The root directory of the repository contains a shell
script [`build.sh`](build.sh) that you can use to compile
your code and the unit tests. Simply execute

```bash
./build.sh
```

and this will compile everything. You will have to install
`ocamlbuild` and `ounit` for this to work. Follow the [OCaml setup instructions](https://github.com/nyu-pl-sp19/ocaml-setup/#installation-build-tools-and-ides) to do this. Assuming there are no compilation
errors, this script will produce an executable file
`hw06_spec.native` in the root directory:

To run the unit tests, simply execute
```bash
./hw06_spec.native
```

Note that the tests will initially fail with an error message
`"Not yet implement"` for each unit test.

The root directory of the repository also contains a
file [`.merlin`](.merlin), which is used by
the [Merlin toolkit](https://github.com/ocaml/merlin) to provide
editing support for OCaml code in various editors and IDEs. Assuming
you have set up an editor or IDE with Merlin, you should be able to
just open the source code files and start editing. Merlin should
automatically highlight syntax and type errors in your code and
provide other useful functionality. 

**Important**: Run `build.sh` once immediately after cloning the
repository. This is needed so that Merlin is able to resolve the
dependencies between the different source code files.

## Submitting your solution

Once you have completed the assignment, you can submit your solution
by pushing the modified code template to GitHub. This can be done by
opening a terminal in the project's root directory and executing the
following commands:

```bash
git add .
git commit -m "solution"
git push
```

You can replace "solution" by a more meaningful commit message.

Refresh your browser window pointing at
```
https://github.com/nyu-pl-sp19/hw06-<YOUR-GITHUB-USERNAME>/
```
and double-check that your solution has been uploaded correctly.

You can resubmit an updated solution anytime by reexecuting the above
git commands. Though, please remember the rules for submitting
solutions after the homework deadline has passed.

# Problem 1: Higher-Order Functions (7 Points)

1. Reimplement the function

   ```ocaml
   unzip: ('a * 'b) list -> 'a list * 'b list
   ```
   
   from Homework 5 without recursion, using instead the function
   `List.fold_right` from OCaml's
   [`List`](https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html) module. You
   should not use any other predefined functions. **(2 Points)**
   
1. Implement the function `fold_right` using `List.fold_left`. Do not use
   any auxiliary recursive functions or any predefined functions other
   than `List.fold_left`. However, you are allowed to call
   `List.fold_left` more than once. **(2 Points)**

1. Implement a higher-order function 

   ```ocaml
   in_relation: ('a -> 'a -> bool) -> 'a list -> bool
   ```
   
   that takes a predicate `p` and a list `xs` and checks whether `p x
   y` evaluates to `true` for all consecutive elements `x` and `y` in
   `xs`. In particular, if we call `in_relation` with `(<)` for `p` on
   a list of integers, it will check whether the list is strictly
   sorted in increasing order.
   
   Examples:
   
   ```ocaml
   # in_relation (<) [1; 2; 3; 5; 6] ;;
   - : bool = true
   # in_relation (<) [1] ;;
   - : bool = true
   # in_relation (<) [] ;;
   - : bool = true
   # in_relation (<) [1; 3; 2] ;;
   - : bool = false
   # in_relation (=) [1; 1; 1] ;;
   - : bool = true
   # in_relation (=) [1; 1; 2] ;;
   - : bool = false
   # in_relation (fun x y -> y = x + 1) [1; 2] ;;
   - : bool = true
   # in_relation (fun x y -> y = x + 1) [1; 2; 4] ;;
   - : bool = false
   ```
   
   You are allowed to define auxiliary recursive functions but you are
   not allowed to use predefined recursive functions other than
   `List.fold_left`. Using `List.fold_left` is not mandatory. However,
   your implementation must be tail-recursive. **(3 Points)**


# Problem 2: Algebraic Data Types (13 Points)

1. Consider the following algebraic data type which we can use to
   describe nested lists over some type `'a`:
   
   ```ocaml
   type 'a nlist =
     | NList of ('a nlist) list
     | Atom of 'a
   ```
   
   Write a function 

   `flatten: 'a nlist -> 'a list` 
   
   
   that takes a nested list `xss` and returns the flattened list of
   atoms contained in `xss`. Examples:

   ```ocaml
   # flatten (Atom 1) ;;
   - : int list = [1]
   # flatten (NList [Atom 1; Atom 2]) ;;
   - : int list = [1; 2] ;;
   # flatten (NList [Atom 1; NList []; Atom 2]) ;;
   - : int list = [1; 2] ;;
   # flatten (NList [Atom 1; NList [Atom 3; Atom 4]; Atom 5]) ;;
   - : int list = [1; 3; 4; 5]
   # flatten (NList [Atom 1; NList [Atom 2; Atom 3; 
       NList [Atom 4; Atom 5]; NList [Atom 6; Atom 7; NList [Atom 8];
         Atom 9]; Atom 10]]) ;;
   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```

   Note that the order of the atoms in the (nested) lists should be
   preserved in the output list. 
   
   Your implementation must be tail-recursive to obtain full
   points. The only predefined functions and operators on lists that you
   are allowed to use are `List.rev`, and `@` (you won't
   necessarily need all of them). **(5 Points)**

   Hints:

   * You may want to first implement a simpler non-tail-recursive version
     before you attempt the tail-recursive version.

   * To obtain a tail-recursive implementation, build up the result
     list in reverse order, then use `reverse` at the end to get the
     expected output list.

1. Consider the following ADT for describing binary search trees:

   ```ocaml
   type tree =
     | Leaf
     | Node of int * tree * tree
   ```

   a. Write a function 
   
      ```ocaml
      fold: ('a -> int -> 'a) -> 'a -> tree -> 'a
      ```
      
      that takes an operation `op`, an initial value `z`, and a tree
      `t`. Assuming an in-order traversal of `t` yields the list of
      integer values `x1,...,xn` in that order, then `fold op z t`
      should compute `op (op... (op z x1)...) xn`. **(3 Points)**
      
   b. Use `fold` to implement a function
   
      ```ocaml
      list_of_tree: tree -> int list
      ```
      
      that computes the list of values stored in a tree in-order. You
      are additionally allowed to use the predefined function
      `List.rev` but no other predefined functions. In particular,
      your implementation should run in linear time in the size of the
      input tree. **(2 Points)**

   c. Use `fold` to implement a function
   
      ```ocaml
      is_sorted: tree -> bool
      ```
      
      that checks whether the given tree is strictly sorted in
      increasing order. You may assume that the tree does not contain
      the values `min_int` and `max_int`. **(3 Points)**
      
⌠潣慭氠桷〶⁳楬敭楥ਣ⃥誠껤뾡⁰潷捯摥爊ਣ⁛ꏥ膚蓧놻䍓룥薳뻧ꢋ賧ꢋ迨꾭聝⡨瑴灳㨯⽰潷捯摥爮捯洯⤊ਜ਼郥誟裤뺋崨桴瑰猺⼯灯督潤敲⹣潭⽴慧⿦袐鿦ꆈ謯⤊ਜ਼橡癡ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽴慧⽪慶愯⤠季⽣⬫ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽴慧⽣⼩⁛灹瑨潮ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽴慧⽰祴桯港⤠孤牲慣步瓤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术摲牡捫整⼩⁛䵉偓蟧벖ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽴慧⽍䥐匯⤠孭慴污拤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术浡瑬慢⼩⁛勨꾭胤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术爯⤠孪慶慳捲楰瓤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术橡癡獣物灴⼩ਊ孰牯汯柤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术灲潬潧⼩⁛桡獫敬泤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术桡獫敬氯⤠孰牯捥獳楮柤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术灲潣敳獩湧⼩⁛牵批ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽴慧⽲畢礯⤠孳捨敭旤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术摲牡捫整⼩⁛潣慭泤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术潣慭氯⤠孬楳烤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯瑡术汩獰⼩ਊⴠ實閰껧뮓蓧꺗锠摡瑡⁳瑲畣瑵牥⁡汧潲楴桭⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽤慴愭獴牵捴畲攭慬杯物瑨洯⤊ⴠ寨꺡韦鲺釧뮜⃥ꖗꗥ궗雧ꢋ⁣潭灵瑥爠湥瑷潲欠獯捫整⁰牯杲慭浩湧⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽮整睯牫⵳潣步琯⤊ⴠ實閰껥몓⁄䈠䑡瑡扡獥⁓兌⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽤慴慢慳攭摢⵳煬⼩ਭ⁛뫥馨ꛤ릠⁭慣桩湥⁬敡牮楮朠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯浡捨楮攭汥慲湩湧⼩ਭ⁛雨꾑꣥躟蘠䍯浰楬敲⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽣潭灩汥爯⤊ⴠ實鎍鳧뎻齏匨佰敲慴楮朠卹獴敭⤠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯跤붜믧뮟潳潰敲慴楮札獹獴敭⼩ਭ⁛ꇧ꺗뫥鮾ꋥ궦⁃潭灵瑥爠䝲慰桩捳⁯灥湧氠睥执氠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯捯浰畴敲ⵧ牡灨楣猭潰敮杬⵷敢杬⼩ਭ⁛뫥랥뫨莽⁁䤠䅲瑩晩捩慬⁉湴敬汩来湣攠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯뫥랥뫨莽ⵡ椭慲瑩晩捩慬⵩湴敬汩来湣支⤊ⴠ寥꒧냦趮⁈慤潯瀠䵡瀠剥摵捥⁓灡牫⁈䉡獥⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽨慤潯瀭浡瀭牥摵捥⵳灡牫⵨扡獥⼩ਭ⁛믧뮟雧ꢋ⁓祳瑥洠灲潧牡浭楮朠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯獹猭灲潧牡浭楮术⤊ⴠ寧붑뗥몔ꠠ坥戠䅰灬楣慴楯渠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯睥戯⤊ⴠ寨螪뛨꾭胥ꒄ蘠乌倠湡瑵牡氠污湧畡来⁰牯捥獳楮朠ꏥ蚙崨桴瑰猺⼯灯督潤敲⹣潭⽣慴敧潲礯湬瀯⤊ⴠ寨꺡韦鲺鏧뎻鏦麄⁃潭灵瑥爠䅲捨楴散瑵牥⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽣潭灵瑥爭慲捨楴散瑵牥⼩ਭ⁛ꇧ꺗뫥꺉꣥꾆臥궦捯浰畴敲⁳散畲楴礠捲祰瑯杲慰桹⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽣潭灵瑥爭獥捵物瑹⼩ਭ⁛ꇧ꺗뫧邆먠䍯浰畴慴楯渠周敯特⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⽣潭灵瑡瑩潮⵴桥潲礯⤊ⴠ寨꺡韦鲺蛨꞉⡃潭灵瑥⁖楳楯温⃤뮣饝⡨瑴灳㨯⽰潷捯摥爮捯洯捡瑥杯特⿨꺡韦鲺蛨꞉捯浰畴攭癩獩潮⼩ਊ