package fr.inria.contraintes.biocham.modelData;

import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.utils.Utils;




public class TopLeftLTLPanelSecond extends JPanel{
	
	
	public TopLeftLTLPanelSecond(LTLController listener){
		
		
		super();
		
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));		
		setBackground(Utils.backgroundColor);
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Checking QFLTL Properties"));	
		setName("Checking QFLTL Properties");
		String toolTipText="<html><i>Computes the domains of the variables that make the formula in argument<br>" +
		                           " true on a numerical trace. The domains are described in the output by a<br>" +
		                           " disjunction of conjonction of inequality constraints over the variables/i></html>";
		CustomToolTipButton b3=new CustomToolTipButton("Check QFLTL Property",toolTipText); 
		b3.setName("computeQFLTL");
		b3.setActionCommand("computeQFLTL");
		b3.addActionListener(listener);
		b3.setBalloonToolTipVisible(false);
			
		toolTipText="<html><i>Computes the satisfaction degree of a QFLTL(R) formula given with the list of its free variables and the list of the objective values.</i></html>";
		
		CustomToolTipButton b4=new CustomToolTipButton("Compute Satisfaction degree",toolTipText);   
		b4.setName("satisfactionDegree");
		b4.setActionCommand("satisfactionDegree");
		b4.addActionListener(listener);
		b4.setBalloonToolTipVisible(false);
		
		toolTipText="<html><i>Compute the satisfaction degree landscape of a QFLTL(R) formula (third argument with list of variables and objective values \n" +
		" in fourth and fifth argument respectively) on a 2D parameter grid saved in a .csv file (last argument). The grid is defined \n " +
		"by a list of two parameters (first argument) given with their range (second argument) scanned with a fixed step (sixth argument).\n" +
		" The seventh argument specifies the time horizon for the simulation.</i></html>";
		CustomToolTipButton b5=new CustomToolTipButton("Get Satisfaction landscape",toolTipText);      
		b5.setName("landscape");
		b5.setActionCommand("landscape");   
		b5.addActionListener(listener);
		b5.setBalloonToolTipVisible(false);

		toolTipText="<html><i>Compute the robustness of the model with respect to a given QFLTL(R) property and a parameter perturbation model.</i></html>";
		CustomToolTipButton b6=new CustomToolTipButton("Compute Robustness",toolTipText);      
		b6.setName("robustness");
		b6.setActionCommand("robustness");   
		b6.addActionListener(listener);
		b6.setBalloonToolTipVisible(false);	
				
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b3);
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b4);
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b5);
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b6);		
	
	}
}
