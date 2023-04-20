# Parser
Parser developed in the Software Maintenance and Evolution course

## How to run:

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
x = pParse "def main(args){a=100;x=0;while(a>b){x++;if(!a>b){a--;}};x=let{a=a+b;}in c;func coco();}"
reverseP x
```