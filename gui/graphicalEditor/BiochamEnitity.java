package fr.inria.contraintes.biocham.graphicalEditor;

import java.util.UUID;

public class BiochamEnitity {

	UUID id;
	String name;
	
	public BiochamEnitity(UUID mid, String mname){
		id=mid;
		name=mname;
	}

	public UUID getId() {
		return id;
	}

	public void setId(UUID id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
