package fr.inria.contraintes.biocham.plotting;

import java.util.List;



public class DatasetTitles {

	String title=null;
	List<String> values;
	
	public DatasetTitles(String t,List<String> vals){
		title=t;
		values=vals;
		
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public List<String> getValues() {
		return values;
	}

	public void setValues(List<String> values) {
		this.values = values;
	}


}
