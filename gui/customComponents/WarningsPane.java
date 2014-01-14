package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Icons;

import java.awt.Graphics;
import java.awt.Image;

import javax.swing.JTextPane;



/**
 * Class thats creates the the plotting area for visualizing the numerical simulation results done by the biocham boolean simulator.
 * 
 * @author Dragana Jovanovska  
 */ 
public class WarningsPane extends JTextPane{

	//Image img = Icons.images.get("rotated_splash.png");
	public WarningsPane(){
		super();
		setOpaque(false);
		//super.setOpaque(false);
	}
	/*@Override
    protected void paintComponent(Graphics g) {
        
		

        g.drawImage(img, 0, 0, this);
        super.paintComponent(g);
    }*/

}
