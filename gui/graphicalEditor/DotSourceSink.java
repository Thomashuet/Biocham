package fr.inria.contraintes.biocham.graphicalEditor;

import java.util.UUID;

public class DotSourceSink {

	String id;
	String reaction;
	String role;
	
	public DotSourceSink(String id1,String reaction1, String role1){
		id=id1;
		reaction=reaction1;
		role=role1;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getReaction() {
		return reaction;
	}
	public void setReaction(String reaction) {
		this.reaction = reaction;
	}
	public String getRole() {
		return role;
	}
	public void setRole(String role) {
		this.role = role;
	}
}
