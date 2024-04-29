import { Injectable } from '@nestjs/common';
import { spawnSync } from 'child_process';

@Injectable()
export class AppService {
  getHello(): string {
    return 'Hello World!';
  }

  getRVersion(): { rversion: string } {
    const buffer = spawnSync('Rscript', ['--version']);
    const rversion = buffer.output.toString();
    return { rversion };
  }
}
