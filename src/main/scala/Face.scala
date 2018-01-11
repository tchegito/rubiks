package tiger.rubiks.model

case class Transformation(face:CubeFace.Value, i1:Int, i2:Int, i3:Int)

case class Pos(x: Int, y: Int)

case class Face(val cubes: Seq[Color.Value]) {

/*
	if (cubes.size = 1) {
		assert(cubes.size==9, "Une face doit avoir 9 cubes !")
	}
	*/
	def this(color: Color.Value) {
		// Tous les cubes de la face ont la même couleur
		this(List.fill(9)(color))
	}
	
	def this(s: String) {
		// On parse une chaîne de caractères au lieu d'une liste
		this(s map (c => Color.withName(c.toString)))
	}
	val correspond = Map(
		(-1,-1)->0, (0,-1)->1, (1,-1)->2,
		(-1,0)->3, (0,0)->4, (1,0)->5,
		(-1,1)->6, (0,1)->7, (1,1)->8)
	
	def cube(p: Pos)= {
		// Return list element corresponding to Pos
		cubeFromIndex(correspond(p.x, p.y))
	}
	
	def setFromIndex(i: Int, col: Color.Value)=
		Face(cubes.updated(i, col))
	
	def cubeFromIndex(i: Int)=
		cubes(i)
	
	private def c(p: (Int, Int))=
		cube(Pos(p._1, p._2))
	
	def rotate(sens: Boolean):Face={
		if (sens)
			Face(List(
				c(-1,1), c(-1,0), c(-1,-1),
				c(0,1), c(0,0), c(0,-1),
				c(1,1), c(1,0), c(1,-1)))
		else
			Face(List(
				c(1,-1), c(1,0), c(1,1),
				c(0,-1), c(0,0), c(0,1),
				c(-1,-1), c(-1,0), c(-1,1)))
	}
	
	def isResolved ={
		// On compte les différentes valeurs => s'il n'y en a qu'une, la face est résolue
		val colorOccurences = cubes groupBy identity mapValues(_.size)
		colorOccurences.size == 1
	}
		
	// Renvoie le nombre de cubes bien placés
	def countWellPlaced={
		val center=cubes(4)
		(cubes filter (_==center)).size
	}
	override def hashCode={
		cubes.map(f=>f.id * 7).sum
	}
	
	override def toString={
		// Display 3 rows of 3 columns
		cubes.mkString //.grouped(3).mkString("\n")
	}
}

object Face {
	// Pour autoriser la surcharge du constructeur sans écrire "new ..."
	def apply(color: Color.Value) = new Face(color)
	
	def apply(s: String) = new Face(s)
}
