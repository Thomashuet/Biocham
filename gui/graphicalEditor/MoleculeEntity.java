package fr.inria.contraintes.biocham.graphicalEditor;


public class MoleculeEntity {

	String name;
	String compartment;	
	MoleculeSize size;
	Position position;
	

	public MoleculeEntity(String name){		
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
	public String getCompartment() {
		return compartment;
	}
	public void setCompartment(String compartment) {
		this.compartment = compartment;
	}
	public MoleculeSize getSize() {
		return size;
	}
	public void setSize(MoleculeSize size) {
		this.size = size;
	}
}
