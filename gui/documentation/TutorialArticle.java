package fr.inria.contraintes.biocham.documentation;



public class TutorialArticle {

			
	private String name;
	private String fileName;
		
	
	public TutorialArticle(String articleName,String fname){
		name=articleName;
		//URL url=this.getClass().getResource("resources/tutorials/"+fname);
		fileName=fname;//url.getFile();
	}
	
	
	
	
	
	public String toString(){
		return name;
	}
	public String getFileName() {
		return fileName;
	}
	public void setFileName(String fileName) {
		this.fileName = fileName;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	
}
