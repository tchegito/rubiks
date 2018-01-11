import org.specs2.mutable.Specification
import tiger.rubiks.model._

object TestCube extends Specification {

	"resolution" should {
		"match where all faces are resolved" in {
			val faceUp = Face(Color.R)
			val faceFront = Face(Color.G)
			val faceRight = Face(Color.Y)
			val faceLeft = Face(Color.O)
			val faceBack = Face(Color.B)
			val faceDown = Face(Color.W)
	
			val cube = Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
			cube.isResolved must be equalTo(true)
			cube.countWellPlaced must be equalTo(54)
		}
	
		"not match where one face isn't resolved" in {
			val faceUp = Face("RRYYRRGGG")
			val faceFront = Face(Color.G)
			val faceRight = Face(Color.Y)
			val faceLeft = Face(Color.O)
			val faceBack = Face(Color.B)
			val faceDown = Face(Color.W)
			
			val cube = Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
			cube.isResolved must be equalTo(false)
			cube.countWellPlaced must be equalTo(54-5)
		}
	}

	"placement" should {
		"be right counted" in {
			val faceUp = Face(Color.R)
			val faceFront = Face("GGGRGGGGG")
			val faceRight = Face(Color.Y)
			val faceLeft = Face(Color.O)
			val faceBack = Face(Color.B)
			val faceDown = Face(Color.W)

			faceUp.countWellPlaced must be equalTo(9)
			faceFront.countWellPlaced must be equalTo(8)

			var cube = Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
			cube.countWellPlaced must be equalTo(9*5 + 8)
			
			cube = cube.rotateFace(CubeFace.R)
			cube.countWellPlaced must be equalTo(9*5 + 8 - 3 - 3 - 3 - 3)
			
			cube = cube.rotateFace(CubeFace.R, false)
			cube.countWellPlaced must be equalTo(9*5 + 8)
			
		}
	}	
	"hashCode" should {
		"be the same after inverse movement" in {
			val faceUp = Face(Color.R)
			val faceFront = Face(Color.G)
			val faceRight = Face(Color.Y)
			val faceLeft = Face(Color.O)
			val faceBack = Face(Color.B)
			val faceDown = Face(Color.W)
			
			var cube = Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
			val h1 = cube.hashCode
			cube = cube.rotateFace(CubeFace.U)
			
			val h2 = cube.hashCode
			h2 must not be equalTo(h1)
			
			// Inverse movement
			cube = cube.rotateFace(CubeFace.U, false)
			val h3 = cube.hashCode
			h3 must not be equalTo(h2)
			h3 must be equalTo(h1)
			
		}
	}
	
	"transformations" should {
		"be ok on cube1" in {
			var cube = makeTestCube
			val command="FL'U"
			cube = cube.applyTransformations(command)
			
			cube.cubeFaces(CubeFace.U).toString must be equalTo("WWWBRGBBG")
		}
		
		"be ok on cube2" in {
			var cube = makeAlmostSolvedCube
			val command="FL'UR"
			cube = cube.applyTransformations(command)
			
			cube.cubeFaces(CubeFace.U).toString must be equalTo("OORGYYBOY")
		}

		"be ok on face" in {
			var face = Face("WGGWRBWRR")
			face = face.rotate(true)
			face.toString must be equalTo("WWWRRGRBG")
		}
	}
		
	"solve" should {
		"be done with given movements" in {
			var cube = makeAlmostSolvedCube
			val command="L'UUL'UUD'LF'LFLLDLLFFULLULLU'FF"
			println(cube.display)
			cube = cube.applyTransformations(command)
			cube.isResolved must be equalTo(true)
		}
	}
	
	def makeTestCube={
		val faceUp = Face("BGGRRBRRR") //RGGRRRRRR")
		val faceFront = Face("WWWWWWWWW")
		val faceRight = Face("GYYGGOGGR") //GGYGGYGGO")
		val faceLeft = Face("OBBYBBYBB") //BBBBBBGBB")
		val faceBack = Face("GRBRYYYYR") //OOYYYRYYR") //YYRYYROOY") //YYOYYORRY")
		val faceDown = Face("OOOOOOOGY") //YOOYOOBOO")

		Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
	}
	
	def makeAlmostSolvedCube={
		val faceUp=Face("GYRBBBBBB")
		val faceFront=Face("WWWWWWWWW")
		val faceRight=Face("RRYRRBRRY").rotate(true)
		val faceLeft=Face("YOOYOOYOO").rotate(true)
		val faceDown=Face("GGGGGGBRR").rotate(false).rotate(false)
		val faceBack=Face("OYGGYYOOB").rotate(false) //BOOYYGGYO")
		
		// On tourne pour se mettre dans la même position que le resolveur de ruwix
		/*
		U <= B
		F <= L
		R <= U
		
		D =>
		L =>
		B =>
		*/
		//Cube( Array(faceUp, faceLeft, faceRight, faceFront, faceDown, faceBack) )
		Cube( Array(faceBack, faceDown, faceUp, faceLeft, faceFront, faceRight) )
	}
	
}