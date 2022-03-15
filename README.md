# Mora Ogunro - 474 HW 3
# How to run the program
1. Download the files and extract them
2. navigate to 474_HW1_SBT/src/main/scala and open SetTheoryDSL.scala in a text editor of your choice
3. open up a terminal or command line and navigate to the project folder within it
4. enter the sbt shell by running the command "sbt" in the terminal
5. run the command "compile" to compile the project files
6. run the "run" command to run the project
7. If successful, you should see a welcome message printed in the sbt shell

To use this language, insert expressions within the method body of "runSetExp()", which is located in SetTheoryDSL.scala. \
After making changes to the file, **save and complete steps 5 and 6 again**. You must do this after every change.

See the **SYNTAX** section below for descriptions of each procedure and how to use them.
# How to run the tests
In the sbt shell, run the command "test"

# Syntax
Optionally, expressions can be wrapped in a Scope. This will set the scope as well as run the expression. \
For example,
```
Scope("default", SetExp)
```
See the Syntax for Scope for a more detailed example.

**AbstractClassDef(name: SetExp, field: SetExp = NoneCase(), constructor: SetExp = NoneCase())**

At least one method must be abstract. To make a method abstract, the expression must be "NoneCase()" or None.
```
AbstractClassDef(
    name = Value("abstractClass"),
    field = Field(Value(("aField1", "private")), Value(("aField2", "public"))),
    constructor = Constructor(Method("initialMethod", NoneCase(), "private"))
    ).eval
```

in this example, initialMethod has a method body of "NoneCase()", which makes it abstract. See Method, field, and constructor syntax for details.

**InterfaceDecl(name: SetExp, field: SetExp = NoneCase(), constructor: SetExp = NoneCase())**

```
InterfaceDecl(
    name = Value("interface1"),
    field = Field(Value(("iField1", "public"))),
    constructor = Constructor(Method("iMethod",NoneCase(),"public"))
    ).eval
```
An interface MUST NOT contain any private fields or methods. All methods must be abstract, meaning the method body (the expression inside of Method) is NoneCase().

**Constructor(exp: SetExp\*)**

```
Constructor(Assign(Variable(Value("a")), Value(99), "public"))
```

**ClassDef(name: SetExp, field: SetExp = NoneCase(), constructor: SetExp = NoneCase())**

_name_ : Value (required) \
*_field_ : Field (optional) \
*_constructor_ : Constructor (optional)

```
ClassDef(
    name = Value("myClass"),
    field = Field(Value(("f", "private")), Value(("a", "public")), Value(("b", "private"))),
    constructor = Constructor(Method("initialMethod", Assign(Variable(Value("f")), Value(2)), "private"), Assign(Variable(Value("a")), Value(99), "public")
    )
```
**Constructor(exp: SetExp\*)**

```
Constructor(Assign(Variable(Value("a")), Value(99), "public"))
```
_exp_ can take multiple expressions at once, given that they are seperated by ",".
For Example,
```
Constructor(
    Assign(Variable(Value("a")), Value(99), "public"), 
    Method("initialMethod", Assign(Variable(Value("f")), Value(2)), "private")
)
```
**Field(expressions: SetExp\*)**

Field requires the user to define access modifiers for each variable. 
```
Field(Value(("f", "private")))

Value(  (variableName, accessModifier)  )
```
_expressions_ can take multiple expressions at once, given that they are seperated by ",".
For Example,
```
Field(
    Value(("f", "private")), 
    Value(("a", "public")), 
    Value(("b", "private"))
)
```
**Method(name: String, exp: SetExp, access: String = ""))**

_name_ : String (required) \
_exp_ : SetExp (required) \
_access_ : String (optional)

if the access modifier is not specified, it will be assigned as public.
```
Method(
    "methodName", 
    Assign(Variable(Value("f")), Value(2)), 
    "private"
)
Method("methodName", Assign(Variable(Value("f")), Value(2))) <- defaults to public
```
**NewObject(className: String, objectName: String)**
```
NewObject("className", "objectName")
```
**InvokeMethod(objectName: String, methodName:String, access:String = "public")**
```
InvokeMethod("objectName", "methodName")
```

**Extends(parentClass: String)**

_Extends()_ is a method that must be called alongside ClassDef: 
```
ClassDef(name = Value("extendedClass")) Extends "myClass"

ClassDef(
    name = Value("myClass"),
    field = Field(Value(("f", "private")), Value(("a", "public")), Value(("b", "private"))),
    constructor = Constructor(Method("initialMethod", Assign(Variable(Value("f")), Value(2)), "private"), Assign(Variable(Value("a")), Value(99), "public")
) Extends "myClass"

```

**Scope(scopeName: String, expression: SetExp)**
```
Scope("default", Union(Variable(Value("setName"))), Variable(Value("setName")))).eval
```
The above command shows how to properly wrap your expression inside a scope. "default" is the name of your desired scope.
Scope will print the variable bindings of the specified scope before and after  each evaluation.

**Value(input: BasicType)**
```
Value(5).eval
```
Will return 5.

**Variable(expr: SetExp)**

Variables must contain a Value expression which holds the variable name.
```
Variable(Value("myVariableName"))).eval
```

**Check(name: SetExp, input: SetExp)**
```
Check(Variable(Value("firstSet")), Value(5)).eval
```
**Assign(name: SetExp, input: SetExp)**
```
Assign(Variable(Value("testSet")), Value(5)).eval
```
**Delete(name: SetExp, input: SetExp)**
```
Delete(Variable(Value("firstSet")), Value(5)).eval
```
**Union(set1:SetExp, set2:SetExp)**
```
Union(Variable(Value("firstSet")), Variable(Value("secondSet"))).eval
```
**Intersection(set1:SetExp, set2:SetExp)**
```
Intersection(Variable(Value("firstSet")), Variable(Value("secondSet")))).eval
```
**SetDifference(set1:SetExp, set2:SetExp)**
```
SetDifference(Variable(Value("firstSet")), Variable(Value("secondSet")))).eval
```
**SymmetricDifference(set1:SetExp, set2:SetExp)**
```
SymmetricDifference(Variable(Value("firstSet")), Variable(Value("secondSet")))).eval
```
**Cartesian(set1:SetExp, set2:SetExp)**
```
Cartesian(Variable(Value("firstSet")), Variable(Value("secondSet")))).eval
```
**Macro(name: String, input: SetExp = NoneCase())**

To create a macro
```
Macro("myMacro", Delete(Variable(Value("firstSet")), Value(1))).eval
```
To use the macro

```Macro("myMacro").eval```

