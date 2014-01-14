package fr.inria.contraintes.biocham.modelData;

import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.utils.Utils;



/**
 * @author Dragana Jovanovska
 *
 */
public class TopLeftLTLPanelZero extends JPanel{


	
	
	public TopLeftLTLPanelZero(LTLController listener){
		
		super();
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));		
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),""));
		setBackground(Utils.backgroundColor);		
		
		String toolTipText="<html><i>LTL(R) formulae are evaluated on a numerical trace either created by simulation or imported from outside with this command...</i></html>";
		
		CustomToolTipButton b5=new CustomToolTipButton("Load Trace",toolTipText);  
		b5.setName("loadTrace");
		b5.setActionCommand("loadTrace");
		b5.addActionListener(listener);
		b5.setBalloonToolTipVisible(false);		
		toolTipText="<html><i>Uses the LTL model-checking techniques to automatize<br>" +
					"the search for parameter values (kinetic parameters or <br>" +
					"initial value parameters) satisfying an LTL specification <br> " +
					"with numerical constraints...</i></html>";
		
		CustomToolTipButton b6=new CustomToolTipButton("Search Parameters",toolTipText);  
		b6.setName("searchParams");
		b6.setActionCommand("searchParams");
		b6.addActionListener(listener);
		b6.setBalloonToolTipVisible(false);					
		add(Box.createRigidArea(new Dimension(15,20)));
		add(b5);
		add(Box.createRigidArea(new Dimension(15,10)));
		add(b6);
	}
}
