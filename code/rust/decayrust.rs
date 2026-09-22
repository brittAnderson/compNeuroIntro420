use plotters::prelude::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parameters
    let y0: f64 = 2.7;      // initial value
    let lambda: f64 = 1.0;  // decay rate

    // Time points: seq(1, 6, by = .01)
    let t: Vec<f64> = (0..)
        .map(|i| 1.0 + i as f64 * 0.01)
        .take_while(|&x| x <= 6.0 + 1e-9)
        .collect();

    // Compute decay: y = y0 * exp(-lambda * t)
    let y: Vec<f64> = t.iter().map(|&ti| y0 * (-lambda * ti).exp()).collect();

    // Combine into (x, y) pairs, like the data frame
    let data: Vec<(f64, f64)> = t.iter().cloned().zip(y.iter().cloned()).collect();

    // Plot
    let root = BitMapBackend::new("decay2.png", (800, 600)).into_drawing_area();
    root.fill(&WHITE)?;

    let y_max = y.iter().cloned().fold(f64::MIN, f64::max);

    let mut chart = ChartBuilder::on(&root)
        .caption("Exponential Decay", ("sans-serif", 30))
        .margin(20)
        .x_label_area_size(40)
        .y_label_area_size(50)
        .build_cartesian_2d(1.0f64..6.0f64, 0.0f64..(y_max * 1.05))?;

    chart
        .configure_mesh()
        .x_desc("Time")
        .y_desc("Value")
        .draw()?;

    chart.draw_series(LineSeries::new(data, &BLUE))?
        .label("value");

    root.present()?;
    println!("Plot saved to decay2.png");

    Ok(())
}
