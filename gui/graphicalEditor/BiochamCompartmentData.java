package fr.inria.contraintes.biocham.graphicalEditor;

import java.awt.Color;
import java.awt.geom.Rectangle2D;

public class BiochamCompartmentData {

	
	String compartmentName;
	String compartmentVolume;
	Object[] containingObjects;
	Position position;
	Color color;
	BiochamGraph graph;
	Rectangle2D bounds;
	
	public BiochamCompartmentData(String name,BiochamGraph g){
		compartmentName=name;
		graph=g;
	}
	public BiochamCompartmentData(BiochamGraph g){		
		graph=g;
	}

	public String getCompartmentName() {
		return compartmentName;
	}

	public void setCompartmentName(String compartmentName) {
		this.compartmentName = compartmentName;
	}

	public String getCompartmentVolume() {
		return compartmentVolume;
	}

	public void setCompartmentVolume(String compartmentVolume) {
		this.compartmentVolume = compartmentVolume;
	}

	public Object[] getContainingObjects() {
		return containingObjects;
	}

	public void setContainingObjects(Object[] containingObjects) {
		this.containingObjects = containingObjects;
	}

	public Position getPosition() {
		return position;
	}

	public void setPosition(Position position) {
		this.position = position;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public BiochamGraph getGraph() {
		return graph;
	}

	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}
	public Rectangle2D getBounds() {
		return bounds;
	}
	public void setBounds(Rectangle2D bounds) {
		this.bounds = bounds;
	}
	
}
