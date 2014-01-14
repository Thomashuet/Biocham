package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
import fr.inria.contraintes.biocham.modelData.EventsModel.Event;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.StringTokenizer;


/**
 * The Controller executes the corresponding methods on EventsView clicking events.
 * It updates the Events model and the Events view, and sends commands to Biocham.
 * 
 * */
public class EventsController implements ActionListener{

	EventsModel eventsModel;
	EventsView eventsView;	
	DeleteEvent deleteListener;
	String timeEvent="time";

	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public EventsController(EventsModel model, EventsView view){
		eventsModel=model;
		eventsView=view;		
		deleteListener=new DeleteEvent();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addEvent")){
			timeEvent=null;
			add();
		}else if(e.getActionCommand().equals("addTimeEvent")){
			timeEvent="time";
			add();
		}	
	}
	
	public void add() {
		DialogAddParameter param=new DialogAddParameter(eventsView.getParentFrame(),eventsModel.getBiochamModel().getEvents(),null,timeEvent,null);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		param=null;				
		//String name=; // name = "condition-paramNames", or "kinetics-condition-paramNames" for time events.
		//String value=; // value = kinetics
		
		
		if((String)newParam[0]!=null && (String)newParam[1]!=null){
			StringBuilder sb=new StringBuilder();
			int ind=0;
			String condition,paramName,kinetics;
			if(timeEvent!=null){
				sb.append("add_time_event(");
				ind=((String)newParam[0]).indexOf(';');		
				sb.append(((String)newParam[0]).substring(0,ind));				
				sb.append(",");				
				int i=(((String)newParam[0]).substring(ind+1)).indexOf(";");
				sb.append(((String)newParam[0]).substring(ind+1,ind+1+i));
				sb.append(",");		
				int j=(((String)newParam[0]).substring(ind+1+i)).indexOf(";");
				sb.append("[");		
				sb.append(((String)newParam[0]).substring(ind+i+2));
				sb.append("]");		
				sb.append(",[");		
				sb.append((String)newParam[1]);				
			}else{
				sb.append("add_event(");
				ind=((String)newParam[0]).indexOf(';');				
				sb.append(((String)newParam[0]).substring(0,ind));
				sb.append(",[");
				sb.append(((String)newParam[0]).substring(ind+1));
				sb.append("],[");
				sb.append((String)newParam[1]);
			}			
			sb.append("]).\n");
			eventsModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;		
			
		}
		
		param=null;
		newParam=null;		
	}

	public void delete(String s){
		
		
		StringBuilder sb=new StringBuilder();
		boolean timeEvent=false;
		Event e=eventsModel.getEvent(s);
		eventsModel.deleteEvent(e);
		
		if(e!=null){
			if(e.getFixedTime()!=null){
				timeEvent=true;
				sb.append("delete_time_event(");
			}else{
				sb.append("delete_event(");
			}
		}else{
			sb.append("delete_event(");
		}		
		sb.append(s);		
		sb.append(").\n");
		eventsModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		
	}

	public EventsModel getEventsModel() {
		return eventsModel;
	}

	public void setEventsModel(EventsModel eventsModel) {
		this.eventsModel = eventsModel;
	}

	public EventsView getEventsView() {
		return eventsView;
	}

	public void setEventsView(EventsView eventsView) {
		this.eventsView = eventsView;
	}

	
	public DeleteEvent getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteEvent deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	
	
	/**
	 * Listener that is responsible for deleting a particular Event
	 * */
	class DeleteEvent extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			delete(name);    
			button=null;
			name=null;
			
			
		}
	}	
}


