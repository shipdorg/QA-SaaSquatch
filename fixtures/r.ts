import { spawnSync } from 'child_process';

export const getSystemVersion = (): string => {
  const buffer = spawnSync('Rscript', ['--version']);
  return buffer.output.toString();
};
