package fr.inria.contraintes.biocham.customComponents;

import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.ModernBalloonStyle;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JLabel;



/**
 * Class thats creates a custom JLabel component with a custom tooltip attached (BalloonTip) for the label explanation.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CustomToolTipLabel extends JLabel implements MouseListener{


	BalloonTip tp;
	String texto="";
	static final Color color1=new Color(153,204,255);//blue, 
	static final Color color2=new Color(230,230,230);//grey
	static final Color color3=new Color(204,204,204);//darker grey
	
		
	public CustomToolTipLabel(String s,String ttt){
		super(s);
				
		addMouseListener(this);
		
		
		if(ttt!=null){
			ModernBalloonStyle modern=new ModernBalloonStyle(4,4,color1,color2,color3);
			modern.setBorderThickness(3);
			modern.enableAntiAliasing(true);
			tp=new BalloonTip(this,ttt,modern,BalloonTip.Orientation.RIGHT_ABOVE,BalloonTip.AttachLocation.NORTHEAST,20,10,false);
			tp.setText(ttt);
			tp.enableClickToClose(true);
			tp.enableClickToHide(true);
			tp.setVisible(false);
			
		}
		
		
			
	}
	public void setToolTipTexto(String s){
		texto=s;
	}
	public void setBalloonToolTipVisible(boolean b){
		if(b){
			tp.setVisible(true);
			
		}else{
			tp.setVisible(false);
		}
	}

	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseEntered(MouseEvent e) {
		if(e.getSource() instanceof CustomToolTipLabel && tp!=null){
			CustomToolTipLabel b=(CustomToolTipLabel)e.getSource();
			b.setBalloonToolTipVisible(true);			
		}
	}
	public void mouseExited(MouseEvent e) {		
		if(e.getSource() instanceof CustomToolTipLabel && tp!=null){
			CustomToolTipLabel b=(CustomToolTipLabel)e.getSource();
			b.setBalloonToolTipVisible(false);
		}
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		if(e.getSource() instanceof CustomToolTipLabel && tp!=null){
			CustomToolTipLabel b=(CustomToolTipLabel)e.getSource();
			b.setBalloonToolTipVisible(false);
		}
		
	}
	


}
