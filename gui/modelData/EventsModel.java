package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamModel;

import java.util.ArrayList;



/**
 * Class that holds the events of a biocham model, and offers its manipulation (like adding,modifying, removing, etc.).
 * contains all of the events data and the logic, or methods and functions, to manipulate the data.
 * 
 * */
public class EventsModel extends ModelData{

	BiochamModel biochamModel;
	ArrayList<Event> events;
	ArrayList<EventsView> views;
	
	/**
	 * A constructor that needs a handle to a biocham model object.	 * 
	 * It initializes the data lists of the class.
	 * */
	public EventsModel(BiochamModel m){
		biochamModel=m;	
		events=new ArrayList<Event>();		
		views=new ArrayList<EventsView>();
	}
	
	/**
	 * Adds a events (name, value) to the list, if its doesn't exist yet in that list. 
	 * */
	public void addEvent(Event  e) {	
		
		if(!events.contains(e)){
			events.add(e);
		}		
	}
	public void addEvent(String s){
		boolean exists=false;
		Event e=null;
		for(int i=0;i<events.size();i++){
			if(events.get(i).toString().equals(s)){
				exists=true;
				e=events.get(i);
				break;
			}
		}
		if(!exists){
			events.add(e);
		}		
	}
	/**
	 * Deletes an event
	 * 
	 * */
	public void deleteEvent(Event e){		
		if(events.contains(e)){
			events.remove(e);
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}
	}	
	public void deleteEvent(String s ){
		for(int i=0;i<events.size();i++){
			if(events.get(i).toString().equals(s)){		
				events.remove(events.get(i));
				break;
			}
		}	
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}		
	}	
	
	
	/**
	 * Deletes all the events.
	 * */
	public void deleteAll(){
		events.clear();
		for(int i=0;i<views.size();i++){
			views.get(i).refresh();
		}	
	}
	
	/**
	 * Returns the biocham model these events belong to.
	 * 
	 **/
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	/**
	 * Sets the biocham model these events belong to.
	 * 
	 **/
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	/**
	 * Returns the list of events in the model.
	 * 
	 **/
	public ArrayList<Event> getEvents() {
		return events;
	}	
	
	/**
	 * Returns a list of the existing views connected to this model.
	 * */
	public ArrayList<EventsView> getViews() {
		return views;
	}
	public Event getEvent(String name){
		for(int i=0;i<events.size();i++){
			if(events.get(i).toString().equals(name)){
				return events.get(i);
			}
		}
		return null;
	}
	
	/**
	 * Data structure for the object Event.
	 * condition; list of parameters, list of kinetics.
	 * 
	 * add_event(Time>=10,[param1,param2,...,paramN],[kine1,kine2,...,kineN])
	 * 
	 * */
	public class Event{
		String condition;
		String parameters, kinetics;
		String eventName=null;
		String fixedTime;
		
		public Event(String time, String c, String paramName, String value){
			fixedTime=time;
			condition=c;
			parameters=paramName;
			kinetics=value;
			StringBuilder sb=new StringBuilder();
			if(time!=null){
				sb.append(fixedTime);
				sb.append(",");	
			}			
			sb.append(c);
			sb.append(",[");
			sb.append(paramName);
			sb.append("],[");
			sb.append(value);
			sb.append("]");
			eventName=sb.toString();			
			sb=null;
		}
		public Event(Object time, String c, String n,
				String k) {
			this(time.toString(),c,n,k);
		}
		
		public String getCondition() {
			return condition;
		}
		public void setCondition(String condition) {
			this.condition = condition;
		}
		public String getParameters() {
			return parameters;
		}
		public void setParameters(String parameters) {
			this.parameters = parameters;
		}
		public String getKinetics() {
			return kinetics;
		}
		public void setKinetics(String kinetics) {
			this.kinetics = kinetics;
		}
		public String toString(){
			return eventName;
		}
		public String getFixedTime() {
			return fixedTime;
		}
		public void setFixedTime(String fixedTime) {
			this.fixedTime = fixedTime;
		}
	}
	
	/**
	 * Disposes the initial states model resources correctly.
	 * */
	public void disposeElement(){
		events.clear();	
		events=null;		
		biochamModel=null;
		views.clear();
		views=null;
	}
}
