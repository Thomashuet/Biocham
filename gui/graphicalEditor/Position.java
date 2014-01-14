package fr.inria.contraintes.biocham.graphicalEditor;

public class Position {

	double x, y, x1, y1;
	
	public Position(String x0, String y0,String x01, String y01){
		x=Double.valueOf(x0);
		y=Double.valueOf(y0);;
		x1=Double.valueOf(x01);;
		y1=Double.valueOf(y01);;
	}
	
	public Position(double x0, double y0,double x01, double y01){
		x=x0;
		y=y0;
		x1=x01;
		y1=y01;
	}

	public Double getX() {
		return x;
	}

	public void setX(Double x) {
		this.x = x;
	}

	public Double getY() {
		return y;
	}

	public void setY(Double y) {
		this.y = y;
	}

	public Double getX1() {
		return x1;
	}

	public void setX1(Double x1) {
		this.x1 = x1;
	}

	public Double getY1() {
		return y1;
	}

	public void setY1(Double y1) {
		this.y1 = y1;
	}
}
