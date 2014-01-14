package fr.inria.contraintes.biocham.modelData;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.utils.Utils;



public class TopLeftCTLPanelSecond extends JPanel{
	
	
	
	public TopLeftCTLPanelSecond(CTLController listener){
		
		super();
		
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));		
		setBackground(Utils.backgroundColor);
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Model Modifications"));	
		setName("Model modifications");
		
		String toolTipText="<html><i>Reduces the model by deleting rules, up to a minimal model <br>" +
		  "that satisfies the whole specification.</i></html>";
		
		CustomToolTipButton b5=new CustomToolTipButton("Reduce Model",toolTipText);  
		b5.setName("reduceModel");
		b5.setActionCommand("reduceModel");
		b5.addActionListener(listener);
		b5.setBalloonToolTipVisible(false);
		
		toolTipText="<html><i>Uses Machine Learning techniques to try and find completions or modifications <br>" +
				  "of a model such that the specification is satisfied.....</i></html>";
		
		CustomToolTipButton b6=new CustomToolTipButton("Learn Rules",toolTipText);  
		b6.setName("learnRules");
		b6.setActionCommand("learnRules");
		b6.addActionListener(listener);
		b6.setBalloonToolTipVisible(false);
		
		toolTipText="<html><i>Tries to correct the model using a theory revision algorithm...</i></html>";
		
		CustomToolTipButton b7=new CustomToolTipButton("Revise Model",toolTipText); 
		b7.setBalloonToolTipVisible(false);
		b7.setName("reviseModel");
		b7.setActionCommand("reviseModel");
		b7.addActionListener(listener); 
		
		add(Box.createRigidArea(new Dimension(20,10)));
		add(b5);
		add(Box.createRigidArea(new Dimension(20,10)));
		add(b6);
		add(Box.createRigidArea(new Dimension(20,10)));
		add(b7);			
	}
}
