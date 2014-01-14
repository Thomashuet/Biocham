package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Utils;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;



/**
 * Class thats creates the the plotting area for visualizing the numerical simulation results done by the biocham boolean simulator.
 * 
 * @author Dragana Jovanovska  
 */ 
public class UniversalScrollPane extends JScrollPane{
	
	public UniversalScrollPane(JComponent view){
		super(view);
		if(Utils.is_OS_MAC){
			setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		}
	}
	
	public UniversalScrollPane(JPanel ii){
		super(ii);	
		if(Utils.is_OS_MAC){
			setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
			setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		}		
	}

}
