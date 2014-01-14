package fr.inria.contraintes.biocham.graphicalEditor;

public class Location {

	double x;
	double y;
	
	public Location(double xx, double yy){
		x=xx;
		y=yy;
	}

	public double getX() {
		return x;
	}

	public void setX(double x) {
		this.x = x;
	}

	public double getY() {
		return y;
	}

	public void setY(double y) {
		this.y = y;
	}
}
