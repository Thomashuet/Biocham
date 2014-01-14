package fr.inria.contraintes.biocham.graphicalEditor;

import java.awt.Dimension;

public class MoleculeSize {

	double width,height;
	
	public MoleculeSize(){
		
	}
	
	public MoleculeSize(double w,double h){
		width=w;
		height=h;
	}

	public MoleculeSize(Dimension d){
		width=d.getWidth();
		height=d.getHeight();
	}
	
	public double getHeight() {
		return height;
	}

	public void setHeight(double height) {
		this.height = height;
	}

	public double getWidth() {
		return width;
	}

	public void setWidth(double width) {
		this.width = width;
	}
}
