package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;



/**
 * Class thats creates the the plotting area for visualizing the numerical simulation results done by the biocham boolean simulator.
 * 
 * @author Dragana Jovanovska  
 */ 
public class GradientPanel extends JPanel{

	public GradientPanel(){
		super();
		setBackground(Utils.backgroundColor);
	}
	
	
	@Override
	public void paintComponent(Graphics g){
		
		if(!isOpaque()){
			super.paintComponent(g);
			return;
		}
		
		Graphics2D g2d=(Graphics2D)g;
		int w=getWidth();
		int h=getHeight();
		GradientPaint gp=new GradientPaint(0,0,getBackground().brighter(),0,h,getBackground());
		g2d.setPaint(gp);
		g2d.fillRect(0,0,w,h);
		
		setOpaque(false);
		super.paintComponent(g);
		setOpaque(true);
	}
}
