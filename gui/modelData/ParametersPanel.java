package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

public class ParametersPanel extends JPanel implements IView{

	String name;
	public ParametersPanel(String name){
		super();
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),name));
		setLayout(new BorderLayout());	
		setBackground(Utils.backgroundColor);
		setName(name);
		this.name=name;
	}

	public void refresh() {
		// TODO Auto-generated method stub
		
	}
	
	public String toString(){
		return name;
	}

	
}
