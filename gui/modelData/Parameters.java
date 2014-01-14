package fr.inria.contraintes.biocham.modelData;


import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;

import java.awt.Color;
import java.awt.Component;
import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public interface Parameters {

	static final int LEFT_OFF = 5, TOP_OFF = 15,HEIGHT = 25, RIGHT_OFF = 320, MIDDLE = 10;
	static final Color VERY_LIGHT_PINK = new Color(255, 230, 230);
	
	public HashMap<String,String> params=new HashMap<String,String>();
	
	public WorkbenchArea biocham=null;
	public JPanel panel=null;
	public BiochamModel model=null;
	public BiochamModelElement element=null;
	//public int getPanelCurrentX();
	//public int getPanelCurrentY();
	/**
	    * Returns the index of the parameter which name is given as an argument.
	    *
	    * @see #Parameter
	*/
	int indexOf(String paramName);
	
	
	/**
	    * Returns the name of the parameter which index is given as an argument.
	    *
	    * @see #Parameter
	*/
	String getName(int i);
	
	/**
	    * Returns the current value of the parameter which index is given as an argument.
	    *
	    * @see #Parameter
	*/
	String getValue(int i);
	
	public void addParameter();
	
	/**
	    * Returns the original value of the parameter which name is the argument of this method.
	*/
	void resetParameter(String s);
	
	
	/**
	    * Returns the number of parameters of the current model.
	*/
	int size();
	
	
	/**
	    * Adds new parameter to the parameters's screen on the gui,
	    * where the list holds the name and the values. 
	*/
	public void setValue(ArrayList<String> list);
	
	public void setModified(Component comp);
	
	JPanel refreshPanel(ArrayList cs);
	
	public void resetSavedResponses();
	
	public void disposeElements();
	
}
