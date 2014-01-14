package fr.inria.contraintes.biocham.graphicalEditor;

import java.util.ArrayList;

import fr.inria.contraintes.biocham.utils.Utils;

public class DotReaction {

	ArrayList<String> elements;
	ArrayList<String> reactants;
	ArrayList<String> products;
	ArrayList<String> modulators;
	String reactionGlyph;
	String sbgnType;
	String middlePosition, leftPosition, rightPosition;
	int x,y;
	
	public void initiateElements(){
		elements=new ArrayList<String>();
		reactants=new ArrayList<String>();
		products=new ArrayList<String>();
		modulators=new ArrayList<String>();
	}
	public ArrayList<String> getElements() {
		return elements;
	}
	public void setElements(ArrayList<String> el) {
		
		Utils.debugMsg("AFTER:");
		this.elements = el;
		for(int i=0;i<elements.size();i++){
			Utils.debugMsg("Element: "+elements.get(i));
		}
	}
	public String getMiddlePosition() {
		return middlePosition;
	}
	public void setMiddlePosition(String position) {
		this.middlePosition = position;
	}
	public String getSbgnType() {
		return sbgnType;
	}
	public void setSbgnType(String sbgnType) {
		this.sbgnType = sbgnType;
	}
	public int getX() {
		return x;
	}
	public void setX(int x) {
		this.x = x;
	}
	public int getY() {
		return y;
	}
	public void setY(int y) {
		this.y = y;
	}
	public ArrayList<String> getModulators() {
		return modulators;
	}
	public void setModulators(ArrayList<String> modulators) {
		this.modulators = modulators;
	}
	public ArrayList<String> getProducts() {
		return products;
	}
	public void setProducts(ArrayList<String> products) {
		this.products = products;
	}
	public ArrayList<String> getReactants() {
		return reactants;
	}
	public void setReactants(ArrayList<String> reactants) {
		this.reactants = reactants;
	}
	public String getReactionGlyph() {
		return reactionGlyph;
	}
	public void setReactionGlyph(String reactionGlyph) {
		this.reactionGlyph = reactionGlyph;
	}
	public String getLeftPosition() {
		return leftPosition;
	}
	public void setLeftPosition(String leftPosition) {
		this.leftPosition = leftPosition;
	}
	public String getRightPosition() {
		return rightPosition;
	}
	public void setRightPosition(String rightPosition) {
		this.rightPosition = rightPosition;
	}
		
}
