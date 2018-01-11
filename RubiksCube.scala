package tiger.rubiks.solve

import tiger.rubiks.model._


case class Transformation(face:CubeFace.Value, i1:Int, i2:Int, i3:Int)

import Color._

object RubiksCube {

	def main(args:Array[String] ):Unit={
		val face:Face = Face(List(
			G, G, R, 
			G, G, G, 
			G, G, G))
			
		val faceUp = Face("BGGRRBRRR") //RGGRRRRRR")
		val faceFront = Face("WWWWWWWWW")
		val faceRight = Face("GYYGGOGGR") //GGYGGYGGO")
		val faceLeft = Face("OBBYBBYBB") //BBBBBBGBB")
		val faceBack = Face("GRBRYYYYR") //OOYYYRYYR") //YYRYYROOY") //YYOYYORRY")
		val faceDown = Face("OOOOOOOGY") //YOOYOOBOO")

/*
		val faceUp = Face("RRRRRRRRR") //RGGRRRRRR")
		val faceFront = Face("WWWWWWWWW")
		val faceRight = Face("YYYYYYYYY") //GGYGGYGGO")
		val faceLeft = Face("BBBBBBBBB") //BBBBBBGBB")
		val faceBack = Face("GGGGGGGGG") //OOYYYRYYR") //YYRYYROOY") //YYOYYORRY")
		val faceDown = Face("OOOOOOOOO") //YOOYOOBOO")
*/
		var cube = Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
		
		cube = makeAlmostSolvedCube
		cube = makeSolvedCube
		println(cube.display)
		//cube = cube.rotate
		
		//println("Rotated="+face.rotate(false))
		
		val belgeDroite="D'R'DRDFD'F'"

		val croixJaune="FRUR'U'F'"
		
		//println(cube.display)
		//cube = cube.rotate
		//cube.rotateFace(CubeFace.R,false)
		
		//println(cube.displayBack)
		//println(cube.displayUp)
		
		//cube.applyTransformations(croixJaune)
		
		//cube = cube.rotateFace(CubeFace.F, false)
		
		//println(cube.display)
		println(cube.isResolved)
		
		print("Commande:")
		//val command = readLine()
		//cube = cube.applyTransformations(command)
		println(cube.display)
		//cube.resolve()

		// SBT 1.1.0 is mandatory if we want the keys displayed when user types
		val command = readLine("Entrez une commande:")
		cube = cube.applyTransformations(command)
		println(cube.display)
		println(cube.displayBack)
	}
	
	def makeAlmostSolvedCube={
		val faceUp=Face("GYRBBBBBB")
		val faceFront=Face("WWWWWWWWW")
		val faceRight=Face("RRYRRBRRY").rotate(true)
		val faceLeft=Face("YOOYOOYOO").rotate(true)
		val faceDown=Face("GGGGGGBRR").rotate(false).rotate(false)
		val faceBack=Face("OYGGYYOOB").rotate(false)

		Cube( Array(faceBack, faceDown, faceUp, faceLeft, faceFront, faceRight) )
	}
	
	def makeSolvedCube=
		Cube(Array(Face(B), Face(O), Face(R), Face(W), Face(G), Face(Y)))
}