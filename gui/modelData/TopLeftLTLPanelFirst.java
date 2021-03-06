package fr.inria.contraintes.biocham.modelData;

import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.utils.Utils;



public class TopLeftLTLPanelFirst  extends JPanel{

	public TopLeftLTLPanelFirst(LTLController listener){

		super();
		
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));		
		setBackground(Utils.backgroundColor);
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Checking LTL Properties"));	
		setName("Checking LTL Properties");
		String toolTipText="<html><i>Checks each formula of the LTL specification against<br>" +
		" a simulation of the given duration. If no duration is provided,<br>" +
		" tests against the latest simulation.....</i></html>";				
		CustomToolTipButton b3=new CustomToolTipButton("Check",toolTipText); 
		b3.setName("CheckLTLSpec");
		b3.setActionCommand("CheckLTLSpec");
		b3.addActionListener(listener);
		b3.setBalloonToolTipVisible(false);			
		toolTipText="<html><i>Evaluates an LTL(R) query on the latest numerical trace.<br> If none exists, one will be generated by numerical_simulation.<br>" +
		"</i></html>";		
		CustomToolTipButton b4=new CustomToolTipButton("Check LTL Property",toolTipText);   
		b4.setName("CheckLTL");
		b4.setActionCommand("CheckLTL");
		b4.addActionListener(listener);
		b4.setBalloonToolTipVisible(false);		
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b3);
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b4);
		add(Box.createRigidArea(new Dimension(15,20)));		
	
	}
}
