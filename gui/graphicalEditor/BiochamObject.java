package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

public class BiochamObject {
	
	int stoichiometry=1;
	Object port;
	String parentName;
	DefaultGraphCell instance;
	
	public BiochamObject(Object port, int coeffOfStoichimetry, String parent, DefaultGraphCell inst){
		this.port=port;
		this.stoichiometry=coeffOfStoichimetry;
		parentName=parent;
		instance=inst;
	}

	public Object getPort() {
		return port;
	}

	public void setPort(Object port) {
		this.port = port;
	}

	public int getStoichiometry() {
		return stoichiometry;
	}

	public void setStoichiometry(int stoichiometry) {
		this.stoichiometry = stoichiometry;
	}

	public String getParentName() {		
		if(parentName==null){
			return "";
		}
		return parentName;
	}

	public void setParentName(String parentName) {
		this.parentName = parentName;
	}

	public DefaultGraphCell getInstance() {
		return instance;
	}

	public void setInstance(DefaultGraphCell instance) {
		this.instance = instance;
	}

}
