package fr.inria.contraintes.biocham.graphicalEditor;

import java.util.ArrayList;

public class NewEntity {

	
	String name, representingName;
	String initialConcentration;
	String type;
	String state;
	int multimerCoef;
	String location;
	String modificationSites;
	ArrayList<ContainingMolecule> containingMolecules;	
	
	public NewEntity(String n, String ic, String s, String t, int mc, String loc){
		name=n;
		state=s;
		initialConcentration=ic;
		type=t;
		multimerCoef=mc;
		location=loc;
	}
	public String getModificationSites() {
		return modificationSites;
	}

	public void setModificationSites(String modificationSites) {
		this.modificationSites = modificationSites;
	}
	public NewEntity(){}

	public String getInitialConcentration() {
		return initialConcentration;
	}

	public void setInitialConcentration(String initialConcentration) {
		this.initialConcentration = initialConcentration;
	}

	public int getMultimerCoef() {
		return multimerCoef;
	}

	public void setMultimerCoef(int multimerCoef) {
		this.multimerCoef = multimerCoef;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public ArrayList<ContainingMolecule> getContainingMolecules() {
		return containingMolecules;
	}

	public void setContainingMolecules(ArrayList<ContainingMolecule> containingMolecules) {
		this.containingMolecules = containingMolecules;
	}
	public String getRepresentingName() {
		return representingName;
	}
	public void setRepresentingName(String representingName) {
		this.representingName = representingName;
	}

	
	
}
