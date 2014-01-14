package fr.inria.contraintes.biocham.graphicalEditor;

import java.io.Serializable;




public class Reaction  implements Serializable{

	
	String name;
	String compartment;	
	Position position;
	ReactionParticipants reactants, products, modulators;	
	String orientation;
	
	
	public Reaction(String name){		
		this.name=name;	
	}
	
	
	

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Position getPosition() {
		return position;
	}
	public void setPosition(Position position) {
		this.position = position;
	}
	public ReactionParticipants getModulators() {
		return modulators;
	}
	public void setModulators(ReactionParticipants modulators) {
		this.modulators = modulators;
	}
	public ReactionParticipants getProducts() {
		return products;
	}
	public void setProducts(ReactionParticipants products) {
		this.products = products;
	}
	public ReactionParticipants getReactants() {
		return reactants;
	}
	public void setReactants(ReactionParticipants reactants) {
		this.reactants = reactants;
	}
	public String getCompartment() {
		return compartment;
	}
	public void setCompartment(String compartment) {
		this.compartment = compartment;
	}
	public String getOrientation() {
		return orientation;
	}
	public void setOrientation(String orientation) {
		this.orientation = orientation;
	}	
}
