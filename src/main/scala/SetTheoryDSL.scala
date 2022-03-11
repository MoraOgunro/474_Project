import SetTheoryDSL.SetExp.*

import scala.collection.immutable.ArraySeq
import scala.collection.{immutable, mutable}
import scala.collection.mutable.{Map, Set}

/*
  Mora Ogunro
*/

/** SetTheoryDSL provides a set theory language for the user to perform actions on sets */


object SetTheoryDSL:
  type BasicType = Any
  /** variableBinding is the default scope. */
  val variableBinding: mutable.Map[String, Any] = mutable.Map[String, Any]()
  /** scopeMap is a collection of variable scopes */
  val scopeMap: mutable.Map[String, mutable.Map[String, Any]] = mutable.Map[String, mutable.Map[String, Any]]("default" -> variableBinding)
  /** the scope that is currently active */
  val currentScopeName: Array[String] = Array("default")
  /** a map of user-defined macro commands */
  val macroBindings: mutable.Map[String, SetExp] = mutable.Map[String, SetExp]()
  /** used as a flag for functions */
  val isObject: Array[Any] = Array(false, "")
  /***/
  val classMap: mutable.Map[String, mutable.Map[String, Any]] = mutable.Map[String, mutable.Map[String, Any]]()
  val interfaceMap: mutable.Map[String, mutable.Map[String, Any]] = mutable.Map[String, mutable.Map[String, Any]]()
  /***/
  val relationshipMap: mutable.Map[String, String] = mutable.Map[String,String]()
  val isClassDef: Array[Boolean] = Array(false)


  enum SetExp:
    case Value(input: BasicType)
    case Variable(name: SetExp)
    case Check(name: SetExp, input: SetExp)
    case Assign(name: SetExp, input: SetExp, access: String = "public")
    case Union(set1: SetExp, set2: SetExp)
    case Intersection(set1: SetExp, set2: SetExp)
    case SetDifference(set1: SetExp, set2: SetExp)
    case SymmetricDifference(set1: SetExp, set2: SetExp)
    case Cartesian(set1: SetExp, set2: SetExp)
    case Scope(scopeName: String, expression: SetExp)
    case Macro(name: String, input: SetExp = NoneCase())
    case Delete(name: SetExp, input: SetExp)
    case NoneCase()
    case ClassDef(name: SetExp, field: SetExp = NoneCase(), constructor: SetExp = NoneCase())
    case Constructor(exp: SetExp*)
    case Field(expressions: SetExp*)
    case Method(name: String, exp: SetExp, access: String = "")
    case NewObject(className: String, objectName: String)
    case InvokeMethod(objectName: String, methodName: String, access: String = "public")
    case AbstractClassDef(name: SetExp, field: SetExp = NoneCase(), constructor: SetExp = NoneCase())
    case InterfaceDecl(name: SetExp, field: SetExp = NoneCase(), constructor: SetExp = NoneCase())

    def eval: BasicType =
      this match {
        /** Returns the value that was passed into it
         *
         * param i a primitive value
         * return the value that was passed into it
         */
        case Value(i) => i

        /** Retrieves the value associated with a variable name from the scopeMap
         *
         * param expr the Value expression holding the variable name
         * return a tuple containing the variable name and its value. The value will be None if
         * the variable does not exist.
         */
        case Variable(expr) =>

          /** name is of type SetExp, it must be converted into a string */
          val n = expr.eval.asInstanceOf[String]
          try {
            /** using currentScopeName to retrieve the appropriate variable bindings
             * scopeMap returns a map of variables, which is used to find the value of variable n
             */
            (n, scopeMap(currentScopeName(0))(n))
          } catch {
            /**
             * The variable n does not exist. Return a tuple containing the variable name and None
             */
            case e: NoSuchElementException =>

              if (!isObject(0).asInstanceOf[Boolean]) {
                println(s"No variable $n exists within scope ${currentScopeName(0)}")
              }
              (n, None)
          }

        /** Tests if the given input exists within the specified set
         *
         * param name the variable name of the set
         * param input the object to be tested
         * return true if the input exists in a set and false if not.
         */
        case Check(name, input) =>

          /** name is a variable. Retrieve the evaluation of this variable, which is a tuple. */
          val result = name.eval.asInstanceOf[(String, BasicType)]
          val key = result._1
          val v = result._2

          /** the set does not exist */
          if (v == None) {
            println("That set does not exist.")
            return false
          }
          /** the set exists and must be recognized as an instance of a mutable set */
          val valueSet = v.asInstanceOf[mutable.Set[BasicType]]
          val objectToCheck = input.eval

          println(s"Checking if $objectToCheck exists in the set $key, which contains $valueSet")
          if (objectToCheck != None) {
            if (valueSet(objectToCheck)) {
              true
            } else {
              false
            }
          } else {
            /** if the input was None for any reason, return None instead of false
             * and allow the calling function interpret the meaning. */
            println(s"$objectToCheck is None.")
            None
          }

        /** Inserts an object into a set. Creates the set if none exists.
         *
         * param name the name of the set
         * param input the object to be inserted
         * return nothing.
         */
        case Assign(name, input, access) =>

          /** the user must call Assign on a variable, not a string */
          if (!name.isInstanceOf[Variable]) {
            println("Assign must be called on a Variable name, not a string literal.\n" +
              "Try using Assign(Variable(Value(variableName))).\nRemember that the variable does not have to exist, as Assign will create one for you." +
              "Refer to the syntax documentation for an example.")
            throw new IllegalArgumentException
          }

          val scope = if (isObject(0).asInstanceOf[Boolean]) {
            val access_modifier: String = if (access == "public" || access == "private" || access == "protected") {
              access
            } else {
              "public"
            }
            getObject(getScope(currentScopeName(0)),isObject(1).asInstanceOf[String])(access_modifier).asInstanceOf[mutable.Map[String, Any]]
          } else {
            getScope(currentScopeName(0))
          }
          /** variable info contains the tuple evaluation of the name variable */
          val variableInfo = name.eval.asInstanceOf[(String, BasicType)]

          /** create the set if it does not exist with the current scope */
          if (scope.contains(variableInfo._1) && classOf[mutable.HashSet[BasicType]].isInstance(scope(variableInfo._1))) {
            println(s"Found Set with key ${variableInfo._1}")
          } else {
            scope(variableInfo._1) = mutable.Set[BasicType]()
            println(s"Creating a new set for variable ${variableInfo._1}.")
          }

          val result = input.eval
          if (classOf[mutable.HashSet[BasicType]].isInstance(result)) {
            /** if the input is a set */
            scope(variableInfo._1) = scope(variableInfo._1).asInstanceOf[mutable.Set[BasicType]] union result.asInstanceOf[mutable.Set[BasicType]]
          } else if (classOf[(String, BasicType)].isInstance(result)) {
            /** if the input is a variable */
            scope(variableInfo._1).asInstanceOf[mutable.Set[BasicType]] += result.asInstanceOf[(String, BasicType)]._2
          } else {
            /** if the input is a value */
            scope(variableInfo._1).asInstanceOf[mutable.Set[BasicType]] += result
          }
          println(s"Object inserted. The set now contains: ${scope(variableInfo._1).asInstanceOf[mutable.Set[BasicType]]}")

        /** Deletes an object from a set
         *
         * param name the variable name of the set
         * param input the object to be deleted
         * return nothing
         */
        case Delete(name, input) =>

          /** retrieve the tuple returned by Variable() */
          val variableInfo = name.eval.asInstanceOf[(String, BasicType)]

          /** if the requested scope exists and it contains the specified set */
          if (classOf[mutable.Map[String, Any]].isInstance(scopeMap(currentScopeName(0)))
            && scopeMap(currentScopeName(0)).contains(variableInfo._1)) {
            println(s"Deleting from set ${variableInfo._1}")
            // This is necessary because the user might use a variable or a value.
            // The evaluation of a variable outputs a tuple of objects, but evaluations of Values outputs a singe object
            val scope = scopeMap(currentScopeName(0))
            try {
              scope(variableInfo._1).asInstanceOf[mutable.Set[BasicType]] -= input.eval.asInstanceOf[(String, BasicType)]._2
            }
            catch {
              case e: _ =>
                scope(variableInfo._1).asInstanceOf[mutable.Set[BasicType]] -= input.eval
            }
          } else {
            scopeMap(currentScopeName(0))(variableInfo._1) = mutable.Set[BasicType]()
            println(s"Did Not Find Set with key ${variableInfo._1}. Nothing was deleted.")
          }

        /** The union of two sets
         *
         * param set1 a set
         * param set2 a set
         * return the union of set1 and set2 as type mutable.Set
         */
        case Union(set1, set2) =>

          /** retrieve the two sets from their variable bindings */
          val sets = (set1.eval.asInstanceOf[(String, BasicType)]._2, set2.eval.asInstanceOf[(String, BasicType)]._2)

          /** set1 and set2 must be recognized as type Set */
          sets._1.asInstanceOf[mutable.Set[BasicType]] union sets._2.asInstanceOf[mutable.Set[BasicType]]

        /** The intersection of two sets
         *
         * param set1 a set
         * param set2 a set
         * return the intersection of set1 and set2 as type mutable.Set
         */
        case Intersection(set1, set2) =>

          /** retrieve the two sets from their variable bindings */
          val sets = (set1.eval.asInstanceOf[(String, BasicType)]._2, set2.eval.asInstanceOf[(String, BasicType)]._2)
          val f = sets._1
          val s = sets._2

          /** set1 and set2 must be recognized as type Set */
          f.asInstanceOf[mutable.Set[BasicType]] intersect s.asInstanceOf[mutable.Set[BasicType]]

        /** The set difference of two sets
         *
         * param set1 a set
         * param set2 a set
         * return the difference of set1 and set2 as type mutable.Set
         */
        case SetDifference(set1, set2) =>

          /** retrieve the two sets from their variable bindings */
          val f = set1.eval.asInstanceOf[(String, BasicType)]._2
          val s = set2.eval.asInstanceOf[(String, BasicType)]._2

          /** set1 and set2 must be recognized as type Set */
          f.asInstanceOf[mutable.Set[BasicType]] diff s.asInstanceOf[mutable.Set[BasicType]]

        /** The symmetric difference of two sets
         *
         * param set1 a set
         * param set2 a set
         * return the symmetric of set1 and set2 as type mutable.Set
         */
        case SymmetricDifference(set1, set2) =>

          /** set1 and set2 must be recognized as type Set */
          /** Symmetric difference is the difference between the union and intersection */
          Union(set1, set2).eval.asInstanceOf[mutable.Set[BasicType]] diff Intersection(set1, set2).eval.asInstanceOf[mutable.Set[BasicType]]

        /** The cartesian product of two sets
         *
         * param set1 a set
         * param set2 a set
         * return the cartesian product of set1 and set2 as type mutable.Set
         */
        case Cartesian(set1, set2) =>

          /** retrieve the two sets from their variable bindings */
          val f = set1.eval.asInstanceOf[(String, BasicType)]._2.asInstanceOf[mutable.Set[BasicType]]
          val s = set2.eval.asInstanceOf[(String, BasicType)]._2.asInstanceOf[mutable.Set[BasicType]]
          val cartesian = mutable.Set[BasicType]()

          /** n squared loop through f and s */
          f.foreach(f_elem => {
            s.foreach(s_elem => {
              cartesian.addOne((f_elem, s_elem))
            })
          })

          cartesian

        /** Sets the current scope and creates on if it does not exist
         *
         * param scopeName the variable name of the scope
         * param expression the SetExp to be evaluated within scope
         * return the evaluated expression
         */
        case Scope(scopeName, expression) =>

          /** set currentScopeName to scopeName */
          currentScopeName(0) = scopeName

          /** if the scope does not exist, create it */
          if (!(scopeMap contains currentScopeName(0))) {
            println(s"Scope ${currentScopeName(0)} does not exist, creating it now...")
            scopeMap(currentScopeName(0)) = mutable.Map[String, Any]()
          }
          try {
            val result = expression.eval
            println(s"\nEvaluations finished. The current scope now looks like:\n${scopeMap(currentScopeName(0))}\n")
            result
          } catch {
            case e: _ =>
              println("\nError. Please check your syntax.\n")
              println(e.printStackTrace())
          }

        /** Creates a macro binding with a given name and expression
         *
         * param name the variable name of the macro
         * param exp the expression to be saved as a macro
         * return nothing
         */
        case Macro(name, exp: SetExp) =>
          if (exp.eval != None) {
            // Then Add this macro to the macro
            println(s"Adding expression to macro bindings.")
            macroBindings(name) = exp
            println(s"Macro bindings now looks like: $macroBindings")
          } else {
            //Run this macro
            macroBindings(name).eval
          }

        /** Defines a class
         *
         * param name the variable name of the class
         * param field the fields to be inserted
         * param constructor the constructor of the class
         *
         * returns the class
         */
        case ClassDef(name: SetExp, field, constructor) =>
          isClassDef(0) = true
          val className: String = name.eval.asInstanceOf[String]
          val newClass: mutable.Map[String, Any] = mutable.Map[String, Any]()

          if (constructor != NoneCase()) {
            newClass.put("constructor", constructor)
          } else {
            newClass.put("constructor", Constructor())
          }

          if (field != NoneCase()) {
            val fields: ArraySeq[SetExp] = field.eval.asInstanceOf[ArraySeq[SetExp]]
            newClass.put("fields", fields)
          } else {
            newClass.put("fields", ArraySeq[SetExp]())
          }
          // place class within scope
          val result = constructor_helper(className, className, newClass("constructor").asInstanceOf[Constructor], newClass("fields").asInstanceOf[ArraySeq[SetExp]])
          //classMap(className) = newClass
          result.put("classType", "concrete")
          classMap(className) = result
          relationshipMap.put(className, "None")
          isClassDef(0) = false
          newClass
        /***/
        case AbstractClassDef(name: SetExp, field, constructor) =>
          isClassDef(0) = true
          val className: String = name.eval.asInstanceOf[String]
          val newClass: mutable.Map[String, Any] = mutable.Map[String, Any]()

          if (constructor != NoneCase()) {
            newClass.put("constructor", constructor)
          } else {
            newClass.put("constructor", Constructor())
          }

          if (field != NoneCase()) {
            val fields: ArraySeq[SetExp] = field.eval.asInstanceOf[ArraySeq[SetExp]]
            newClass.put("fields", fields)
          } else {
            newClass.put("fields", ArraySeq[SetExp]())
          }
          // place class within scope
          val result = constructor_helper(className, className, newClass("constructor").asInstanceOf[Constructor], newClass("fields").asInstanceOf[ArraySeq[SetExp]])
          if(!checkAbstractClass(result)){
            throw new AbstractMethodError("Abstract classes must contain at least one abstract method")
          }
          result.put("classType", "abstract")
          classMap(className) = result
          relationshipMap.put(className, "None")
          isClassDef(0) = false
          newClass

        case InterfaceDecl(name, field, constructor) =>
          val methodList = constructor.eval
          isClassDef(0) = true
          val interfaceName: String = name.eval.asInstanceOf[String]
          val newInterface: mutable.Map[String, Any] = mutable.Map[String, Any]()

          if (constructor != NoneCase()) {
            newInterface.put("constructor", constructor)
          } else {
            newInterface.put("constructor", Constructor())
          }

          if (field != NoneCase()) {
            val fields: ArraySeq[SetExp] = field.eval.asInstanceOf[ArraySeq[SetExp]]
            newInterface.put("fields", fields)
          } else {
            newInterface.put("fields", ArraySeq[SetExp]())
          }
          // place class within scope
          val result = constructor_helper(interfaceName, interfaceName, newInterface("constructor").asInstanceOf[Constructor], newInterface("fields").asInstanceOf[ArraySeq[SetExp]])
          if(!checkInterface(result, interfaceName)){
            throw new Error("Interfaces must not contain any implementations or private fields.")
          }
          result.put("classType", "interface")
          interfaceMap(interfaceName) = result
          relationshipMap.put(interfaceName, "None")
          isClassDef(0) = false
          newInterface

        /** Retrieves the array of values held in the Field datatype
         *
         * param expressions the array of expressions
         * return an ArraySeq[SetExp]
         */
        case Field(expressions*) =>
          expressions

        /** Retrieves the array of values held in the Constructor datatype
         *
         * param expressions the array of expressions
         * return an ArraySeq[SetExp]
         */
        case Constructor(exp*) =>
          exp

        /** Inserts a method into an object
         *
         * param variable name the method
         * param exp the expression to be inserted
         * param access the access modifier of the method
         *
         * returns nothing
         */
        case Method(name, exp, access) =>
          val access_modifier: String = if (access == "public" || access == "private" || access == "protected") {
            access
          } else {
            "public"
          }

          val currentClass = getObject(getScope(currentScopeName(0)), isObject(1).asInstanceOf[String])
          val classScope = currentClass(access_modifier).asInstanceOf[mutable.Map[String, Any]]

          classScope.put(name, exp);

        /** initializes a new object according to the specified class
         *
         * param className the variable name of the class
         * param objectName the variable name of the object
         * returns nothing
         */
        case NewObject(className, objectName) =>
          val classType = getClass(className)("classType")
          if(classType == "abstract"){
            throw new Error("Abstract Classes cannot be instantiated.")
          }
          val constructor = getClass(className)("constructor")
          val fields = getClass(className)("fields")

          /** constructor_helper does most of the work */
          constructor_helper(className, objectName, constructor.asInstanceOf[Constructor], fields.asInstanceOf[ArraySeq[SetExp]])

        /** Invokes the method in the specified object
         *
         * param objectName the variable name of the object
         * param methodName the variable name of the method
         * param access the access modifier of the method (optional)
         *
         * returns nothing
         */
        case InvokeMethod(objectName, methodName, access) =>
          val scope = getScope(currentScopeName(0))
          val classObject = getObject(scope, objectName)

          /** find and retrieve the method from the object */
          /** search through the public, private, and protected scopes */
          val method: SetExp = {
            val publicMap = classObject("public").asInstanceOf[mutable.Map[String, Any]]
            val privateMap = classObject("private").asInstanceOf[mutable.Map[String, Any]]
            val protectedMap = classObject("protected").asInstanceOf[mutable.Map[String, Any]]
            if (publicMap.contains(methodName)) {
              publicMap(methodName).asInstanceOf[SetExp]
            }
            else if (privateMap.contains(methodName)) {
              privateMap(methodName).asInstanceOf[SetExp]
            }
            else if (protectedMap.contains(methodName)) {
              protectedMap(methodName).asInstanceOf[SetExp]
            }
            else {
              println("Method Does Not Exist")
              return
            }

          }
          isObject(0) = true
          isObject(1) = objectName
          method.eval
          isObject(0) = false
          isObject(1) = ""

        /** NoneCase case used by various expressions
         *
         * return None
         */
        case NoneCase() =>
          None
      }

    /** builds a new class tha inherits from another
     *
     * param parentClass the variable name of the parent class
     *
     * returns nothing
     */
    infix def Extends(parentClass: String): Any = {
      // Extract all information from the parent class
      val originalExpression = this
      /** getNewExpression combines the constructor and fields of the parent class and the child class */

      val newExpression = if(classOf[ClassDef].isInstance(originalExpression)) {
        getNewExpression(originalExpression.asInstanceOf[ClassDef], parentClass)
      }else if(classOf[InterfaceDecl].isInstance(originalExpression)){
        //throw new Error("Interface cannot implement another interface.")
        getNewExpression(originalExpression.asInstanceOf[InterfaceDecl], parentClass)
      }else{
        getNewExpression(originalExpression.asInstanceOf[AbstractClassDef], parentClass)
      }
      val className = newExpression._1
      val constructor = newExpression._2
      val fields = newExpression._3
      // Call ClassDef on the new fields and constructor
      if(classOf[AbstractClassDef].isInstance(originalExpression)){
        AbstractClassDef(className, fields, constructor).eval
      }else if(classOf[InterfaceDecl].isInstance(originalExpression)){
        InterfaceDecl(className, fields, constructor).eval
      }else{
        ClassDef(className, fields, constructor).eval
      }
      relationshipMap.put(className.eval.asInstanceOf[String], parentClass)
      if(getClass(parentClass)("classType") == "abstract"){
        // check if the derived class is implemented properly
        checkAbstractImplementation(getClass(className.eval.asInstanceOf[String]), className.eval.asInstanceOf[String])
      }
    }

    infix def Implements(parentInterface: String): Any = {
      // Extract all information from the parent class
      val originalExpression = this
      /** getNewExpression combines the constructor and fields of the parent class and the child class */

      val newExpression = if(classOf[ClassDef].isInstance(originalExpression)) {
        getNewExpression(originalExpression.asInstanceOf[ClassDef], parentInterface)
      }else if(classOf[InterfaceDecl].isInstance(originalExpression)){
        throw new Error("Interface cannot implement another interface.")
        //getNewExpression(originalExpression.asInstanceOf[InterfaceDecl], parentInterface)
      }else{
        getNewExpression(originalExpression.asInstanceOf[AbstractClassDef], parentInterface)
      }
      val className = newExpression._1
      val constructor = newExpression._2
      val fields = newExpression._3
      // Call ClassDef on the new fields and constructor
      if(classOf[AbstractClassDef].isInstance(originalExpression)){
        AbstractClassDef(className, fields, constructor).eval
      }else if(classOf[InterfaceDecl].isInstance(originalExpression)){
        InterfaceDecl(className, fields, constructor).eval
      }else{
        ClassDef(className, fields, constructor).eval
      }
      relationshipMap.put(className.eval.asInstanceOf[String], parentInterface)

        // check if the derived class is implemented properly
      checkInterfaceImplementation(getClass(className.eval.asInstanceOf[String]), className.eval.asInstanceOf[String])


    }

    def hasCycle(start: String) : Boolean = {

      val visited : mutable.Set[Any] = mutable.Set()
      val cur : Array[Any] = Array(start)

      while(cur(0) != None && !visited.contains(cur(0))){
        if(cur(0) == "None"){
          return false
        }
        visited.addOne(cur(0).asInstanceOf[String])
        cur(0) = relationshipMap(cur(0).asInstanceOf[String])
      }
      true
    }
    /*
     * HELPER METHODS
     */
    def checkAbstractImplementation(derivedClass: mutable.Map[String,Any], className: String): Boolean ={
      val classType: String = derivedClass("classType").asInstanceOf[String]
      if(hasCycle(className)){
        classMap.remove(className)
        relationshipMap.remove(className)
        throw new Error(s"There is a cycle of inheritance.")
      }

      val allMethodsAndFields = getAllMethodsAndFieldsAsList(derivedClass)
      if(classType != "abstract"){
        // check that all methods are implemented
        allMethodsAndFields.foreach( info => {
          if(info._2 == NoneCase()) {
            classMap.remove(className)
            relationshipMap.remove(className)
            throw new AbstractMethodError("All abstract methods must be implemented in a concrete class")
          }
        })
      }

      true
    }
    def checkInterfaceImplementation(derivedClass: mutable.Map[String,Any], className: String): Boolean ={
      val classType: String = derivedClass("classType").asInstanceOf[String]

      if(hasCycle(className)){
        classMap.remove(className)
        relationshipMap.remove(className)
        throw new Error(s"There is a cycle of inheritance.")
      }

      val allMethodsAndFields = getAllMethodsAndFieldsAsList(derivedClass)
      if(classType == "concrete"){
        // check that all methods are implemented
        allMethodsAndFields.foreach( info => {
          if(info._2 == NoneCase()){
            classMap.remove(className)
            relationshipMap.remove(className)
            throw new Error(s"All Interface methods must be implemented in a concrete class. $className has unimplemented method ${info._1}.")
          }
        })
      }else if(classType == "interface"){
        allMethodsAndFields.foreach( info => {
          if(info._2 != None && info._2 != NoneCase()){
            classMap.remove(className)
            relationshipMap.remove(className)
            throw new Error(s"Interfaces must be completely abstract $className implements ${info._1}")
          }
        })
      }else if(classType == "abstract"){
        allMethodsAndFields.foreach( info => {
          if(info._2 == None || info._2 == NoneCase()){
            return true
          }
        })
        classMap.remove(className)
        relationshipMap.remove(className)
        throw new Error(s"Abstract class must have one abstract field or method. $className has only concrete methods and fields.")
      }


      true
    }
    def checkInterface(interface: mutable.Map[String,Any], interfaceName: String): Boolean ={

      val allFieldsAndMethods = getAllMethodsAndFieldsAsMap(interface)
      val privateAccess = allFieldsAndMethods._2
      if(privateAccess.nonEmpty){
        throw new Error("Interface method/field.")
      }

      val fieldAndMethodList = getAllMethodsAndFieldsAsList(interface)

      fieldAndMethodList.foreach( info => {
        if( info._2 != None && info._2 != NoneCase()){
          throw new Error(s"Interfaces must be completely abstract. ${info._1} is not abstract.")
        }

      })
      true
    }
    /** Returns a tuple of a key array and value array of every field or method in the class */
    def getAllMethodsAndFieldsAsList(currentClass: mutable.Map[String,Any]) : List[(String, Any)] = {
      val publicAccess = currentClass("public").asInstanceOf[mutable.Map[String,Any]]
      val privateAccess = currentClass("private").asInstanceOf[mutable.Map[String,Any]]
      val protectedAccess = currentClass("protected").asInstanceOf[mutable.Map[String,Any]]
      val keyList : List[String] = publicAccess.keys.toList ++ privateAccess.keys.toList ++ protectedAccess.keys.toList
      val valueList : List[Any] = publicAccess.values.toList ++ privateAccess.values.toList ++ protectedAccess.values.toList
      //mutable.Map() ++ (keyList zip valueList)
      val a = keyList zip valueList
      a
    }
    def getAllMethodsAndFieldsAsMap(currentClass: mutable.Map[String,Any]) : (mutable.Map[String, Any], mutable.Map[String, Any], mutable.Map[String, Any]) = {
      val publicAccess = currentClass("public").asInstanceOf[mutable.Map[String,Any]]
      val privateAccess = currentClass("private").asInstanceOf[mutable.Map[String,Any]]
      val protectedAccess = currentClass("protected").asInstanceOf[mutable.Map[String,Any]]
      //mutable.Map() ++ (keyList zip valueList)
      (publicAccess, privateAccess, protectedAccess)
    }
    /** builds a new object
     *
     * param className the variable name of the object's class
     * param varName the variable name of the new object
     * param constructor the constructor of the new object
     * param fields the fields to be inserted
     *
     * returns nothing
     */
    def constructor_helper(className: String, objectName: String, constructor: Constructor, fields: ArraySeq[SetExp]): mutable.Map[String,Any] = {
      // set isClass flag to true so that the results of any SetExp in the constructor are placed in the appropriate object
      isObject(0) = true
      isObject(1) = objectName
      val scope = getScope(currentScopeName(0))
      // make a new object containing the class's constructor and insert the object into outer scope
      val newObject = mutable.Map[String, Any]("public" -> mutable.Map[String, Any](), "private" -> mutable.Map[String, Any](), "protected" -> mutable.Map[String, Any](), "constructor" -> constructor)
      scope.put(objectName, newObject)

      // for each Value in fields, insert it into the new object
      fields.foreach(field => {
        val name = field.eval.asInstanceOf[(String, String)]._1
        val access = field.eval.asInstanceOf[(String, String)]._2
        newObject(access).asInstanceOf[mutable.Map[String, Any]].put(name, None)
      })
      // for each expression in the constructor, evaluate it
      val expressions: ArraySeq[SetExp] = constructor.eval.asInstanceOf[ArraySeq[SetExp]]
      expressions.foreach(ex => {
        ex.eval
      })

      newObject.put("type", className)
      // reset flag
      isObject(0) = false
      isObject(1) = ""
      if(isClassDef(0)){
        scope.remove(objectName)
        newObject.put("fields", fields)
      }
      newObject
    }
    def getNewExpression(classDef: ClassDef, parentClassName: String): (Value, SetExp, SetExp) = {
      // get parent class and all it contains

      val parentClass = getClass(parentClassName)
      val parentConstructor: Constructor = if (parentClass("constructor") != NoneCase()) {
        parentClass("constructor").asInstanceOf[Constructor]
      } else {
        Constructor()
      }
      val parentFields: ArraySeq[SetExp] = if (parentClass("fields") != NoneCase()) {
        parentClass("fields").asInstanceOf[ArraySeq[SetExp]]
      } else {
        ArraySeq[SetExp]()
      }

      // extract child constructor, fields, and values
      val childName = classDef.name.asInstanceOf[Value]
      val childConstructor = if (classDef.constructor != NoneCase()) {
        classDef.constructor.asInstanceOf[Constructor]
      } else {
        Constructor()
      }
      val childFields = if (classDef.field != NoneCase()) {
        classDef.field.eval.asInstanceOf[ArraySeq[SetExp]]
      } else {
        ArraySeq[SetExp]()
      }

      val newConstructor: Constructor = {
        val unpackedParentConstructor = parentConstructor.eval.asInstanceOf[ArraySeq[SetExp]]
        val unpackedChildConstructor = childConstructor.eval.asInstanceOf[ArraySeq[SetExp]]
        val combinedArrayOfConstructors: ArraySeq[SetExp] = unpackedParentConstructor ++ unpackedChildConstructor

        Constructor(combinedArrayOfConstructors.distinct: _*)
      }
      val newField = {
        val unpackedParentFields = parentFields
        val unpackedChildFields = childFields
        val combinedArrayOfFields: ArraySeq[SetExp] = unpackedParentFields ++ unpackedChildFields
        Field(combinedArrayOfFields: _*)
      }

      (childName, newConstructor, newField)
    }
    def getNewExpression(classDef: AbstractClassDef, parentClassName: String): (Value, SetExp, SetExp) = {
      // get parent class and all it contains

      val parentClass = getClass(parentClassName)
      val parentConstructor: Constructor = if (parentClass("constructor") != NoneCase()) {
        parentClass("constructor").asInstanceOf[Constructor]
      } else {
        Constructor()
      }
      val parentFields: ArraySeq[SetExp] = if (parentClass("fields") != NoneCase()) {
        parentClass("fields").asInstanceOf[ArraySeq[SetExp]]
      } else {
        ArraySeq[SetExp]()
      }

      // extract child constructor, fields, and values
      val childName = classDef.name.asInstanceOf[Value]
      val childConstructor = if (classDef.constructor != NoneCase()) {
        classDef.constructor.asInstanceOf[Constructor]
      } else {
        Constructor()
      }
      val childFields = if (classDef.field != NoneCase()) {
        classDef.field.eval.asInstanceOf[ArraySeq[SetExp]]
      } else {
        ArraySeq[SetExp]()
      }

      val newConstructor: Constructor = {
        val unpackedParentConstructor = parentConstructor.eval.asInstanceOf[ArraySeq[SetExp]]
        val unpackedChildConstructor = childConstructor.eval.asInstanceOf[ArraySeq[SetExp]]
        val combinedArrayOfConstructors: ArraySeq[SetExp] = unpackedParentConstructor ++ unpackedChildConstructor

        Constructor(combinedArrayOfConstructors.distinct: _*)
      }
      val newField = {
        val unpackedParentFields = parentFields
        val unpackedChildFields = childFields
        val combinedArrayOfFields: ArraySeq[SetExp] = unpackedParentFields ++ unpackedChildFields
        Field(combinedArrayOfFields: _*)
      }

      (childName, newConstructor, newField)
    }
    def getNewExpression(classDef: InterfaceDecl, parentClassName: String): (Value, SetExp, SetExp) = {
      // get parent class and all it contains

      val parentClass = getClass(parentClassName)
      val parentConstructor: Constructor = if (parentClass("constructor") != NoneCase()) {
        parentClass("constructor").asInstanceOf[Constructor]
      } else {
        Constructor()
      }
      val parentFields: ArraySeq[SetExp] = if (parentClass("fields") != NoneCase()) {
        parentClass("fields").asInstanceOf[ArraySeq[SetExp]]
      } else {
        ArraySeq[SetExp]()
      }

      // extract child constructor, fields, and values
      val childName = classDef.name.asInstanceOf[Value]
      val childConstructor = if (classDef.constructor != NoneCase()) {
        classDef.constructor.asInstanceOf[Constructor]
      } else {
        Constructor()
      }
      val childFields = if (classDef.field != NoneCase()) {
        classDef.field.eval.asInstanceOf[ArraySeq[SetExp]]
      } else {
        ArraySeq[SetExp]()
      }

      val newConstructor: Constructor = {
        val unpackedParentConstructor = parentConstructor.eval.asInstanceOf[ArraySeq[SetExp]]
        val unpackedChildConstructor = childConstructor.eval.asInstanceOf[ArraySeq[SetExp]]
        val combinedArrayOfConstructors: ArraySeq[SetExp] = unpackedParentConstructor ++ unpackedChildConstructor

        Constructor(combinedArrayOfConstructors.distinct: _*)
      }
      val newField = {
        val unpackedParentFields = parentFields
        val unpackedChildFields = childFields
        val combinedArrayOfFields: ArraySeq[SetExp] = unpackedParentFields ++ unpackedChildFields
        Field(combinedArrayOfFields: _*)
      }

      (childName, newConstructor, newField)
    }
    def getClass(className: String): mutable.Map[String, Any] = {
      if(classMap.contains(className)) {
        classMap(className)
      }else{
        interfaceMap(className)
      }
    }
    def getObject(scope: mutable.Map[String, Any], objectName: String): mutable.Map[String, Any] = {
      scope(objectName).asInstanceOf[mutable.Map[String, Any]]
    }
    def printScope(scopeName: String): Any = {
      println(s"The $scopeName scope:")
      println(scopeMap(scopeName))
    }
    def printInterfaces: Any = {
      println("Interfaces: ")
      println(interfaceMap)
    }
    def printClasses: Any = {
      println("Classes: ")
      println(classMap)
    }
    def getScope(scopeName: String): mutable.Map[String, Any] = {
      scopeMap(scopeName)
    }
    def checkAbstractClass(abstractClass: mutable.Map[String,Any]) : Boolean = {
      val publicAccess = abstractClass("public").asInstanceOf[mutable.Map[String,Any]]
      val privateAccess = abstractClass("private").asInstanceOf[mutable.Map[String,Any]]
      val protectedAccess = abstractClass("protected").asInstanceOf[mutable.Map[String,Any]]
      // If the class contains a method with no expressions, return true. Else Return False
      publicAccess.values.exists(_ == NoneCase()) || privateAccess.values.exists(_ == NoneCase()) || protectedAccess.values.exists(_ == NoneCase())
    }

@main def runSetExp(): Unit =
  println("***Welcome to my Set Theory DSL!***")
  println("***Please insert your expressions in the main function***\n")
  // Place your expressions here. View README.md for syntax documentation
  AbstractClassDef(Value("myClass"),
    field = Field(Value(("f", "private"))),
    constructor = Constructor(Method("initialMethod", NoneCase(), "private"), Assign(Variable(Value("a")), Value(99), "tiki"))).eval
//  NewObject("myClass","badObject").eval
//
//  InterfaceDecl(
//      name = Value("interface1"),
//      field = Field(Value(("field1", "public")),Value(("field2", "public"))),
//      constructor = Constructor(Method("method1",NoneCase(),"public"))
//    ).eval
//  ClassDef(Value("myClass"),
//    Field(Value("HELLO","private")),
//    constructor = Constructor(Method("method1", Assign(Variable(Value("f")), Value(2)), "public"))) Implements "interface1"
//  InterfaceDecl(
//      name = Value("interface1"),
//      field = Field(Value(("field1", "public")),Value(("field2", "public"))),
//      constructor = Constructor(Method("method1",NoneCase(),"public"))
//    ).eval
  AbstractClassDef(Value("C"),
    field = Field(Value(("c", "public"))),
    constructor = Constructor()) Extends "A"
  AbstractClassDef(Value("B"),
    field = Field(Value(("b", "public"))),
    constructor = Constructor()) Extends "C"
  AbstractClassDef(Value("A"),
    field = Field(Value(("a", "public"))),
    constructor = Constructor()) Extends "B"
  // A,B,C,C
  //NewObject("myClass","badObject").eval
//  InterfaceDecl(
//      name = Value("interface2"),
//      field = Field(Value(("field3", "public"))),
//      constructor = Constructor()
//    ) Extends "interface1"

  Value(1).printScope("default")
  Value(1).printClasses
  Value(1).printInterfaces
