package fr.inria.contraintes.biocham.graphicalEditor;



import org.jgraph.graph.DefaultGraphCell;

import java.awt.Color;
import java.util.UUID;

public class ContainingMolecule {

	
	String name;
	String state;
	int cardinality;
	boolean modulator;
	String moleculeType;
	String initialConcentration;
	String representingName;
	Color color=null;
	UUID id;
	DefaultGraphCell instance;
	BiochamEntityData userObject;
	int reactionStoichiometry=1;
	
	/*public ContainingMolecule(UUID id){
		this.id=id;
	}*/
	public ContainingMolecule(String n, String st, int card, boolean mod, String molType,String ic,String rn,UUID idd,DefaultGraphCell inst){
		name=n;
		state=st;
		cardinality=card;
		modulator=mod;
		moleculeType=molType;
		initialConcentration=ic;
		representingName=rn;
		id=idd;
		instance=inst;
	}
	
	
	
	public int getCardinality() {
		return cardinality;
	}
	public void setCardinality(int cardinality) {
		this.cardinality = cardinality;
	}
	public boolean isModulator() {
		return modulator;
	}
	public void setModulator(boolean modulator) {
		this.modulator = modulator;
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

	public String getMoleculeType() {
		return moleculeType;
	}

	public void setMoleculeType(String moleculeType) {
		this.moleculeType = moleculeType;
	}

	public String getInitialConcentration() {
		return initialConcentration;
	}

	public void setInitialConcentration(String initialConcentration) {
		this.initialConcentration = initialConcentration;
	}

	public String getRepresentingName() {
		return representingName;
	}

	public void setRepresentingName(String representingName) {
		this.representingName = representingName;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public UUID getId() {
		return id;
	}

	public void setId(UUID id) {
		this.id = id;
	}



	public DefaultGraphCell getInstance() {
		return instance;
	}



	public void setInstance(DefaultGraphCell instance) {
		this.instance = instance;
	}



	public BiochamEntityData getUserObject() {
		if(userObject==null){
			userObject=(BiochamEntityData)instance.getUserObject();
		}
		return userObject;
	}



	public void setUserObject(BiochamEntityData userObject) {
		this.userObject = userObject;
	}



	public int getReactionStoichiometry() {
		return reactionStoichiometry;
	}



	public void setReactionStoichiometry(int reactionStoichiometry) {
		this.reactionStoichiometry = reactionStoichiometry;
	}

}
