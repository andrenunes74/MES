# Software Maintenace & Evolution

Parser, case generator testing, mutant generator and property-based testing developed in the Software Maintenance and Evolution course.

## How to run:

### Parser

* Compile your main file:
```bash
ghci tools.hs
```

* Parse your expression:
```haskell
pParse "<your expression>"
```

* Unparse your expression (pretty printing):
```haskell
reverseP (pParse "<your expression>")
```

* Example:

```haskell
x = pParse "def main(args){a=100;x=0;while(a>b){x++;if(!a>b){a--;};};let{a=a+b;}in c;func coco();return a;}"
reverseP x
```

### Generator

* Compile the generator file:
```bash
ghci generator.hs
```

* Generate your trees by simply typing:
```haskell
gen
```

### Mutant generator

* Compile the mutant generator file:
```bash
ghci mutantGen.hs
```

* Apply a specific mutant to your tree:
```haskell
applyRandomMutant <your mutant> <your tree>
```

* Apply a random mutant to your tree:
```haskell
applyRandomMutant <your tree>
```

### Property-based Testing

* Compile the property-based testing file:
```bash
ghci testing.hs
```

* Simply test your properties:
```haskell
quickCheck prop_<prop number>
```