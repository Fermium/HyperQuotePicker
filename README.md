# HyperQuotePicker

Choosing the right quote from the right supplier is damn hard.

Given some quotes from various suppliers for the same pieces, this software computes the most affordable combinations at various numbers of suppliers.

To do so, it computes all possible combinations. From a computing standpoint, this is tremendously inefficient. It's also tremendously good at saving you money and time.

# Input

The software takes in input a .csv file with the following header and content:

| Piece | Price | Quantity | Tot | Supplier | Terms |
| --- | --- | --- | --- | --- | --- |
| screw_m4 | 3.05 | 50 | 152.50 |"Shenzhen Optical Eq"| 50-50 |
| screw_m4 | 4.30 | 50 | 215.00 |"Advanced Hardware Factory"| Pay in Advanced | 

And it takes some assumption:

* The name of the same supplier and same piece are well written and identical
* You want all the pieces, each one from only one supplier
* Each line by itself can satisfy your need for that item
* You really want that amount of pieces


Please note: the quantities and unit price are ignore. Only the total price of the piece matters. They're just there for convenience.

# Output

It output a file with the best three results for 1 to n suppliers and the relative price.

# usage

Create your csv file following the example one. If needed change the name in the config parameters editing the script.



```R
source("qutePicker.R")
```

# This software is slow, why?

Because it is. Computing all possible combinations is a O(n!) problem, one of the slowest possible types. You're going to need a powerful computer even for 20 suppliers and 5 pieces. We usually run it on a 32 core machine.

The code can be adapted with just a couple of lines to run in a cluster.

It's also not optimized. Currently, the heavy lifting is done using data.frame, a very inefficient datatype.
