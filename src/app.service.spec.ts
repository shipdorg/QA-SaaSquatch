import { Test } from '@nestjs/testing';
import { getSystemVersion } from '../fixtures/r';
import { AppService } from './app.service';

describe('AppService', () => {
  const rversion = getSystemVersion();
  let appService: AppService;

  beforeAll(async () => {
    const module = await Test.createTestingModule({
      providers: [AppService],
    }).compile();

    appService = module.get(AppService);
  });

  describe('getRVersion', () => {
    it('should return the current R version', () => {
      expect(appService.getRVersion()).toEqual({ rversion });
    });
  });
});
