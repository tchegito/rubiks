package tiger.rubiks.model

case class Cube(cubeFaces: Map[CubeFace.Value, Face]) {
	// U - L - R - F - D - B

	private val patternViewFront = """
    a
   d b
  g e c
1 \h f/ C
42 \i/ BF
753 | AEI
 86 | DH
  9 | G"""	
  
  private val patternViewBack = """
    c
   b f
  a e i
7 \d h/ A
84 \g/ DB
951 | GEC
 62 | HF
  3 | I"""
	def this(faces: Array[Face]) {
		// On initialise une map indexée par CubeFace avec les faces du tableau d'entrée
		this(faces.zipWithIndex.map ( t=> CubeFace(t._2) -> t._1) toMap)
	}
	
	def rotateFace(cf: CubeFace.Value, sens: Boolean=true):Cube = {
		// Face principale
		//val rotatedCubeFaces = cubeFaces map identity
		//(cubeFaces map { case (k,v) => v }).toArray
		var rotatedCubeFaces = cubeFaces.updated(cf, cubeFaces(cf).rotate(sens))
		// Effets de bord sur les 4 faces connectées
		var facesAround=CubeFace.around(cf)
		//println("face before "+cubeFaces(cf))
		if (!sens) {
			facesAround = facesAround.reverse
		}
		val facesAroundPlusFirst = facesAround :+ facesAround.head
		var extracted:Seq[Color.Value] = Seq()

		for (t <- facesAroundPlusFirst) {
			var face = rotatedCubeFaces(t.face)
			if (!extracted.isEmpty) {
				val before = Seq(face.cubeFromIndex(t.i1), face.cubeFromIndex(t.i2), face.cubeFromIndex(t.i3))
				//println("Modify face "+f+" with "+extracted)
				face = face.setFromIndex(t.i1, extracted(0))
							.setFromIndex(t.i2, extracted(1))
							.setFromIndex(t.i3, extracted(2))
				rotatedCubeFaces = rotatedCubeFaces.updated(t.face, face)
				//println("face="+face)
				extracted = before
			} else {
				extracted = Seq(face.cubeFromIndex(t.i1), face.cubeFromIndex(t.i2), face.cubeFromIndex(t.i3))
			}
		}
		//println(Console.WHITE+"rotate "+cf+(if (sens) "" else "'")+Cube(rotatedCubeFaces).display)
		Cube(rotatedCubeFaces)
		//println("face down="+cubeFaces(CubeFace.D))
	}

	// On bascule le cube pour mettre la face de derrière vers le haut
	def rotate = {
		cubeFaces(CubeFace.R).rotate(false)
		cubeFaces(CubeFace.L).rotate(true)
		Cube(Array(cubeFaces(CubeFace.B),
			 cubeFaces(CubeFace.L),
			 cubeFaces(CubeFace.R),
			 cubeFaces(CubeFace.U),
			 cubeFaces(CubeFace.F),
			 cubeFaces(CubeFace.D)))
	}
	
	def applyTransformations(s: String):Cube= {
		val splitter = "([D|U|F|B|L|R])([']?)".r
		val transfos = (splitter findAllIn s).toList
		var cube = this
		transfos map {
			case splitter(face, "") => cube=cube.rotateFace(CubeFace.withName(face), true)
			case splitter(face, regular) => cube=cube.rotateFace(CubeFace.withName(face), false)
			case _ =>
		}
		//println(Console.WHITE+transfos.size+" transformations appliquées")
		cube
	}
	
	def countWellPlaced:Int =
		(cubeFaces map {case (k,v)=> v.countWellPlaced}).sum
		
	// On affiche une simili vue 3D
	def display =
		displayFaces(Seq(CubeFace.U, CubeFace.F, CubeFace.R), patternViewFront)

	def displayBack =
		displayFaces(Seq(CubeFace.L, CubeFace.B, CubeFace.D), patternViewBack)
		
	def displayUp =
		displayFaces(Seq(CubeFace.B, CubeFace.U, CubeFace.R), patternViewFront)
		
	def displayFaces(faces: Seq[CubeFace.Value], pattern: String):String= {

		val patUp = "[a-i]"
		val patForward = "[1-9]"
		val patRight = "[A-I]"
		//Console.BLUE
		val projected = pattern map {
			case up      if (up.toString.matches(patUp)) => cubie(faces(0), up-'a')
			case forward if (forward.toString.matches(patForward)) => cubie(faces(1), forward-'1')
			case right   if (right.toString.matches(patRight)) => cubie(faces(2), right-'A')
			case c => c
		}
		(projected.replaceAll("G", Console.GREEN+"G")
		.replaceAll("B", Console.BLUE+"B")
		.replaceAll("Y", Console.YELLOW+"Y")
		.replaceAll("O", Console.MAGENTA+"O")
		.replaceAll("W", Console.WHITE+"W")
		.replaceAll("""\/""", Console.WHITE+"/")
		.replaceAll("""\|""", Console.WHITE+"|")
		.replaceAll("R", Console.RED+"R")
		)+Console.WHITE
	}

	def cubie(ff: CubeFace.Value, c: Int):Char=
		cubeFaces(ff).cubeFromIndex(c).toString.head
		
	def isResolved={
		val resolvedFaces=cubeFaces map {case (k,v) => v.isResolved}
		// Si l'une des faces n'est pas résolues, alors c'est non !
		!(resolvedFaces.toList contains false)
	}
		
	private def checkHeuristic(cnt: Int, wellPlaced:Seq[Int])={
	
		//if (wellPlaced.size > 5 && cnt < wellPlaced(0)) {
		//	false
		//}
		if (wellPlaced.size > 10 && cnt < (wellPlaced(5)+2)) {
			false
		}
		if (wellPlaced.size > 20 && cnt < (wellPlaced(10)+2)) {
			false
		}
		if (wellPlaced.size > 30 && cnt < (wellPlaced(20)+2)) {
			false
		}
		if (wellPlaced.size > 40 && cnt < (wellPlaced(30)+2)) {
			false
		}
		if (wellPlaced.size > 50 && cnt < (wellPlaced(40)+2)) {
			false
		}
		if (wellPlaced.size > 60 && cnt < (wellPlaced(50)+4)) {
			false
		}
		
		true
	}
		
		//3*le même => on bloque
		
	// TODO: Ca n'a pas de sens :il s'agit d'une méthode de classe mais qui prend un cube en paramètre
	// Soit elle doit enlever ce paramée, soit elle devient statique
	def resolve(cube: Cube=this, hashCodes: Seq[Int]=Seq(hashCode), movements: Seq[String]=Seq(), wellPlaced:Seq[Int]=Seq(countWellPlaced)):Boolean={
		val cnt = cube.countWellPlaced
		if (cnt > 305 || movements.mkString.startsWith("L'UUL'UU")) //wellPlaced(0))
			println(movements.mkString+" ("+movements.size+") "+cnt+" "+cube.isResolved)
		//println(hashCodes)

		// On essaie de résoudre le cube
		if (cube.isResolved) {
			println("Ca y est ! en "+hashCodes.size+" mouvements !")
			System.exit(1)
			return true
		} else {
			if (wellPlaced.size == 29) { // On abandonne si + de n mouvements
				//println("Stop !")
				return false
			}
			if (!checkHeuristic(cnt, wellPlaced)) {
				return false
			}
			try {
				var lastDouble:String = ""
				if (movements.size > 2) {
					val last = movements.last
					if (movements.takeRight(2).filter(f=>f==last).size == 2)
						lastDouble = last
				}

				var forbiddenElement = Seq[String]()
				if (movements.size > 3) {
					// On interdit les patterns A B A B
					forbiddenElement = forbiddenElement :+ (movements.takeRight(3) match {
						case a :: b :: c :: nil if a==c && a!=b => b
						case _ => ""
					})
					// On interdit les patterns A B B A
					forbiddenElement = forbiddenElement :+ (movements.takeRight(3) match {
						case a :: b :: c :: nil if b==c && a!=b => a
						case _ => ""
					})
				}
				for (m <- CubeFace.values; inverse <- Seq(true,false)) {
					//println(m + (if (inverse) "" else "'"))
					val newMove = m.toString + (if (inverse) "'" else "")
					
					/*if (movements.mkString.startsWith("L'UUL'U")) {
						println(movements+" => "+newMove)
					}*/
					var forbidden=false
					if (lastDouble == newMove || (forbiddenElement contains newMove)) {
						//println("On tourne en rond !")
						forbidden=true
					}
					if (!forbidden) {
						val rotCube = cube.rotateFace(m, !inverse)
						if (hashCodes contains rotCube.hashCode) {
							//println("deja vu")
							//return false
						} else {
							
							if (hashCodes.size < 3) {
								// Premier niveau: on créé un thread par combinaison
								new Thread(new Runnable {
								  def run() {
									//println("Thread !")
									if (resolve(rotCube, hashCodes :+ rotCube.hashCode, 
											movements :+ newMove,
											wellPlaced :+ rotCube.countWellPlaced))
										return true
								  }
								}).start
							} else {
								// On continue dans le même thread
								if (resolve(rotCube, hashCodes :+ rotCube.hashCode, 
											movements :+ newMove,
											wellPlaced :+ rotCube.countWellPlaced))
									return true
							}
						}
					}
				}
			} catch {
				case e: Exception => println("Crac !")
			}
			false
		}
	}
	
	override def hashCode={
		cubeFaces.map(f=>f.hashCode * 49).sum
	}
	
	override def toString={
		(for ((f,v) <- cubeFaces)
			yield "\n"+f+" contient\n"+v).mkString
	}
}

object Cube {
	def apply(cf: Array[Face]) = new Cube(cf)
}
