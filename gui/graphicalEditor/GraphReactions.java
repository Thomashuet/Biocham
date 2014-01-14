package fr.inria.contraintes.biocham.graphicalEditor;

public class GraphReactions {

	String originalRule;
	String modifiedRule;
	
	public GraphReactions(String a, String b){
		originalRule=a;
		modifiedRule=b;
	}

	public String getModifiedRule() {
		return modifiedRule;
	}

	public void setModifiedRule(String modifiedRule) {
		this.modifiedRule = modifiedRule;
	}

	public String getOriginalRule() {
		return originalRule;
	}

	public void setOriginalRule(String originalRule) {
		this.originalRule = originalRule;
	}
	
	
}
