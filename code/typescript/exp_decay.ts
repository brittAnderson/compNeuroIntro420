import * as vega from 'vega';
import * as vl from 'vega-lite';
import fs from 'fs';

// x values from 0 to 6
const xValues: number[] = Array.from({ length: 61 }, (_, i) => i / 10);

// y = e^(-x)
const points: { x: number; y: number }[] = xValues.map(x => ({ x, y: Math.exp(-x) }));

const spec: vl.TopLevelSpec = {
  $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
  title: 'Exponential Decay: y = e^(-x)',
  width: 700,
  height: 450,
  background: 'white',
  data: { values: points },
  mark: { type: 'line', point: { color: 'hotpink' }, color: 'hotpink', strokeWidth: 3 },
  encoding: {
    x: { field: 'x', type: 'quantitative', title: 'x' },
    y: { field: 'y', type: 'quantitative', title: 'y' },
  },
};

async function createChart(): Promise<void> {
  const view = new vega.View(vega.parse(vl.compile(spec).spec), { renderer: 'none' });
  const svg = await view.toSVG();
  fs.writeFileSync('exp_decay.svg', svg);
  console.log('Saved exp_decay.svg');
}

createChart().catch(console.error);
