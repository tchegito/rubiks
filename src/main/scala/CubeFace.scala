package tiger.rubiks.model

object CubeFace extends Enumeration {
	// Forward - Left - Right - Up - Down - Bottom
	val U, L, R, F, D, B = Value
	
	def around(c:CubeFace.Value):Seq[Transformation]=
		c match {
			case U => Seq(Transformation(B, 8, 7, 6),
						  Transformation(R, 0, 1, 2),
						  Transformation(F, 0, 1, 2),
						  Transformation(L, 0, 1, 2))
			case D => Seq(Transformation(L, 6, 7, 8),
						  Transformation(F, 6, 7, 8),
						  Transformation(R, 6, 7, 8),
						  Transformation(B, 2, 1, 0))
			case L => Seq(Transformation(U, 0, 3, 6),
						  Transformation(F, 0, 3, 6),
						  Transformation(D, 0, 3, 6),
						  Transformation(B, 0, 3, 6))
			case R => Seq(Transformation(B, 2, 5, 8),
						  Transformation(D, 2, 5, 8),
						  Transformation(F, 2, 5, 8),
						  Transformation(U, 2, 5, 8))
			case F => Seq(Transformation(U, 6, 7, 8),
						  Transformation(R, 0, 3, 6),
						  Transformation(D, 2, 1, 0),
						  Transformation(L, 8, 5, 2))
			case B => Seq(Transformation(L, 0, 3, 6),
						  Transformation(D, 6, 7, 8),
						  Transformation(R, 2, 5, 8),
						  Transformation(U, 0, 1, 2))
		}
}
