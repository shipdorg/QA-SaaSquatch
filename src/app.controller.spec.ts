import { Test, TestingModule } from '@nestjs/testing';
import { getSystemVersion } from '../fixtures/r';
import { AppController } from './app.controller';
import { AppService } from './app.service';

describe('AppController', () => {
  const rversion = getSystemVersion();
  let appController: AppController;

  beforeAll(async () => {
    const app: TestingModule = await Test.createTestingModule({
      controllers: [AppController],
      providers: [AppService],
    }).compile();

    appController = app.get<AppController>(AppController);
  });

  describe('root', () => {
    it('should return "Hello World!"', () => {
      expect(appController.getRVersion()).toEqual({ rversion });
    });
  });
});
