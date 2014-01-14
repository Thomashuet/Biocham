package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.event.GraphSelectionEvent;

import java.util.Map;

public abstract class BiochamCellData implements org.jgraph.event.GraphSelectionListener{

	Map properties;
	String name;
	final static int MIN_NUMBER_OF_PROPERTIES=3;
	
	
	public Object getProperty(Object key){
		return properties.get(key);
	}
	
	public Object putProperty(Object key, Object value){
		if(value!=null){
			return properties.put(key,value);
		}
		return null;
	}
	
	public Map getProperties(){
		return properties;
	}
	
	public void setProperties(Map props){
		this.properties=props;
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
		putProperty("name",name);
	}
	
	public abstract void valueChanged(GraphSelectionEvent e);

	
	
	
}
