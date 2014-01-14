package fr.inria.contraintes.biocham.graphicalEditor;

public class ReactionProperties {

	
	String originName, actualName;
	Object[] children;
	
	public ReactionProperties(){}
	public ReactionProperties(String oN, String aN, Object[] ch){
		originName=oN;
		actualName=aN;
		children=ch;
	}

	public Object[] getChildren() {
		return children;
	}
	public void setChildren(Object[] children) {
		this.children = children;
	}
	
	public String getOriginName() {
		if(originName==null){
			originName=actualName;
		}
		return originName;
	}
	public void setOriginName(String originName) {
		this.originName = originName;
	}
	public String getActualName() {
		return actualName;
	}
	public void setActualName(String actualName) {
		this.actualName = actualName;
	}
	
	
}
